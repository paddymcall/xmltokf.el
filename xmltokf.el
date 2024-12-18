;;; xmltokf.el --- Functional wrappers around xmltok  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Patrick McAllister

;; Author: Patrick McAllister
;; Keywords: text, hypermedia, languages, XML
;; URL: https://github.com/paddymcall/xmltokf.el
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Uses xmltok without setting global variables (functional, sort of).

;; To run the tests: make test

;;; Usage:

;; Use ‘xmltokf-scan-here’ to construe a token, and
;; ‘xmltokf-scan-element’ to construe an element-ish sort of structure.

;; Functions that don’t end in an “!” aim to be free of any
;; side-effects: they do *not* set global variables (see
;; ‘xmltok-save’), or move the point, but just return the current
;; state.  Kind of purely functional in that respect.

;;  To scan through a document, call something like this:

;; (defun xmltokf-scan-doc ()
;;   (let ((current-token (xmltokf-scan-here (point-min)))
;;         stack)
;;     (while (and current-token (xmltokf-token-type current-token))
;;       ;; Do your stuff...
;;       (push current-token stack)
;;       (setq current-token (xmltokf-scan-here (xmltokf-token-end current-token))))
;;     (nreverse stack)))

;; Packaging:

;; Use `flycheck-mode', and run `package-lint-current-buffer'
;; `checkdoc-current-buffer' from time to time.

;;; Code:

(require 'xmltok)
(require 'dom)
(require 'subr-x)
(require 'cl-lib)
(require 'cl-macs)
;; Probably better in a separate file:
(require 'ert)

;; Implent token and elements with ‘cl-defstruct’: it is included in
;; emacs, and seems to be pretty standard.  Other options considered
;; were ‘eieio’ (also in GNU/Emacs) and ‘record’ (which is actually
;; the basis for ‘cl-defstruct’, according to Info node
;; `(elisp)Records').

(cl-defstruct (xmltokf-token (:type list)
                             :named)
  "A structure to represent a token as scanned by `xmltokf-scan-here'.

See Info node `(cl)Structures' for the general idea."
  (type
   nil
   :documentation "Like `xmltok-type'

Possible xmltok-types (this is what I grepped in `xmltok.el' and
found in practice):

- cdata-section
- comment
- data
- empty-element
- end-tag
- not-well-formed
- partial-empty-element
- partial-end-tag
- partial-start-tag
- space
- start-tag
- prolog --> only if parsing is done by `nxml-token-after'

Also possible: nil, e.g., if `xmltok-forward' is called at end of buffer.")
  (start
   nil
   :documentation  "Like `xmltok-start': start position of the token")
  (name-colon
   nil
   :documentation "Like `xmltok-name-colon': nil or a position if
  there’s a name-prefix")
  (name-start
   nil
   :documentation "Where the element name begins (after the
   colon-separated prefix, if any).")
  (name-end
   nil
   :documentation "Like `xmltok-name-end': nil or a position if there’s a name-prefix")
  (replacement
   nil
   :documentation "Like `xmltok-replacement': “String containing
   replacement for a character or entity reference.”")
  (attributes
   nil
   :type list
   :documentation "Like `xmltok-attributes'")
  (namespace-attributes
   nil
   :type list
   :documentation "Like `xmltok-namespace-attributes'")
  (errors
   nil
   :documentation "Like `xmltok-errors'")
  (end
   nil
   :documentation "Position where `xmltok-forward start' lands
   the pointer.  Could be end of a token, but need not
   be (errors, incomplete, etc.)")
  (buffer
   nil
   :documentation "Buffer that this token belongs to."))

;; (make-xmltokf-token)

(cl-defstruct (xmltokf-element
               (:type list)
               :named)
  "An element as defined by xmltokf."
  type
  start
  end
  (start-token
   nil
   :documentation "Start-token, an ‘xmltokf-token’")
  (end-token
   nil
   :documentation "End-token, an ‘xmltokf-token’: will be nil if there’s no end-tag."))

;; An easier way to switch to the right context; mostly copied from ‘with-current-buffer’
(defmacro xmltokf-with-current-buffer (pom-tok-buff &rest body)
  "Execute the forms in BODY with buffer POM-TOK-BUFF temporarily current.

POM-TOK-BUFF must be one of the following:

- a point: switch to the current buffer (i.e., no change)
- a marker: switch to the marker’s buffer
- an ‘xmltokf-token’: switch to the token’s buffer
- a buffer or the name of an existing buffer

The value returned is the value of the last form in BODY.  See
also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  `(save-current-buffer
     (set-buffer (cond
                  ((markerp ,pom-tok-buff) (marker-buffer ,pom-tok-buff))
                  ((xmltokf-token-p ,pom-tok-buff)
                   (xmltokf-token-buffer ,pom-tok-buff))
                  ((bufferp ,pom-tok-buff) ,pom-tok-buff)
                  (t (current-buffer))))
     ,@body))

(defun xmltokf-scan-here (pom-or-token)
  "Scan token at point, marker, or ‘xmltokf-token’ POM-OR-TOKEN in BUFF.

Calling ‘xmltokf-scan-here’ on an ‘xmltokf-token’ should return
the same token, unless the buffer has changed in that area.

Returns a ‘cl-defstruct’ as defined by ‘make-xmltokf-token’ (see
Info node `(cl)Structures' for the general idea of these
structures)."
  (xmltokf-with-current-buffer pom-or-token
    (let ((start (cond
                  ((markerp pom-or-token) pom-or-token)
                  ((integerp pom-or-token) pom-or-token)
                  ((xmltokf-token-p pom-or-token)
                   (xmltokf-token-start pom-or-token))
                  (t (error "Wrong type of argument for pom-or-token: %s"
                            (type-of pom-or-token))))))
      (save-excursion
        (save-match-data
	  (goto-char start)
          (xmltok-save
            ;; set up the scan data
            (xmltok-forward)
            (cond
	     ((not xmltok-type) nil)
             ((eq xmltok-type 'not-well-formed)
	      ;; This could have been a prolog, check:
	      (cond
	       ;; If there are any errors on the list, don’t bother
	       ;; continuing
	       (xmltok-errors
		(error "Not well formed around position %s"
		       pom-or-token))
	       ((and
		 (goto-char start)
		 (xmltok-forward-prolog)
		 (null xmltok-errors))
		(make-xmltokf-token
		 :type 'prolog
		 :start xmltok-start
		 :name-colon nil
		 :name-start nil
		 :name-end nil
		 :replacement nil
		 :attributes nil
		 :namespace-attributes nil
		 :errors xmltok-errors
		 :end (point)
		 :buffer (current-buffer)))))
	     ((eq xmltok-type 'processing-instruction)
	      (make-xmltokf-token
	       :type xmltok-type
	       :start xmltok-start
	       :name-colon nil
               :name-start (1+ xmltok-start)
	       :name-end xmltok-name-end
	       :replacement nil
	       :attributes nil
	       :namespace-attributes nil
	       :errors xmltok-errors
	       :end (point)
               :buffer (current-buffer)))
             (t
              (make-xmltokf-token
	       :type xmltok-type
	       :start xmltok-start
	       :name-colon xmltok-name-colon
               :name-start (cond
                            ((member xmltok-type '(start-tag empty-element))
                             (1+ (or xmltok-name-colon
	                             xmltok-start)))
                            ((eq xmltok-type 'end-tag)
                             (if xmltok-name-colon
                                 (1+ xmltok-name-colon)
                               (+ 2 xmltok-start)))
                            (t nil))
	       :name-end xmltok-name-end
	       :replacement xmltok-replacement
	       :attributes xmltok-attributes
	       :namespace-attributes xmltok-namespace-attributes
	       :errors xmltok-errors
	       :end (point)
               :buffer (current-buffer))))))))))

;; (with-temp-buffer
;;     (insert "</beep>")
;;     (xmltokf-scan-here (xmltokf-scan-here (point-min))))


;; (ert "test-xmltokf-scan-here")

(defun xmltokf-tag-qname (token)
  "Get tag name for TOKEN (an ‘xmltokf-token’), including prefix (if any).

Only meaningful for start tags, end tags, and empty elements.
Can also accept a processing-instruction.  Returns nil if the
function is not applicable or the prefix is not there."
  (xmltokf-with-current-buffer token
    (let ((ttype (xmltokf-token-type token)))
      (cond
       ((member ttype '(start-tag empty-element))
        (buffer-substring-no-properties
         (1+ (xmltokf-token-start token))
         (xmltokf-token-name-end token)))
       ((member ttype '(end-tag processing-instruction))
        (buffer-substring-no-properties
         (+ 2 (xmltokf-token-start token))
         (xmltokf-token-name-end token)))
       (t nil)))))


;; (ert "test-xmltokf-tag-qname")

(defun xmltokf-tag-prefix (token)
  "Get prefix of TOKEN as a string: tei:p -> tei."
  (when (xmltokf-token-name-colon token)
    (let ((qname (xmltokf-tag-qname token)))
      (when (stringp qname)
        (car (split-string qname ":"))))))


;; (ert "test-xmltokf-tag-prefix")

(defun xmltokf-tag-local-name (token)
  "Get local name of TOKEN as a string: tei:p -> p."
  (let ((qname (xmltokf-tag-qname token)))
    (when (stringp qname)
      (car (nreverse (split-string qname ":"))))))

(defalias 'xmltokf-start-tag-local-name #'xmltokf-tag-local-name)


;; (ert "test-xmltokf-tag-local-name")

;;; These are the same as in xmltok, though since they don't touch the
;;; environment the att might be nil.

(defsubst xmltokf-attribute-name-start (att)
  "Get start position of the name of attribute ATT."
  (and att (aref att 0)))

(defsubst xmltokf-attribute-name-colon (att)
  "Get position of the colon in the name of attribute ATT (or nil)."
  (and att (aref att 1)))

(defsubst xmltokf-attribute-name-end (att)
  "Get start position of attribute name of ATT."
  (and att (aref att 2)))

(defsubst xmltokf-attribute-value-start (att)
  "Get start position of the value of attribute ATT."
  (and att (aref att 3)))

(defsubst xmltokf-attribute-value-end (att)
  "Get end position of the value of attribute ATT."
  (and att (aref att 4)))

(defsubst xmltokf-attribute-raw-normalized-value (att)
  "Return an object representing the normalized value of ATT.

This can be t indicating that the normalized value is the same as
the buffer substring from the start to the end of the value, or
nil indicating that the value is not well-formed, or a string."
  (and att (aref att 5)))

(defsubst xmltokf-attribute-refs (att)
  "Return a list of the entity and character references in ATT.

Each member is a vector [TYPE START END] where TYPE is either char-ref
or entity-ref and START and END are integers giving the start and end of
the reference.  Nested entity references are not included in the list."
  (and att (aref att 6)))

;;; <--- end quote

(defun xmltokf-attribute-prefix (att)
  "Return prefix of attribute ATT or nil."
  (let ((colon (xmltokf-attribute-name-colon att)))
    (and colon
	 (buffer-substring-no-properties
          (xmltokf-attribute-name-start att)
	  colon))))

(defun xmltokf-attribute-local-name (att)
  "Return local-name of attribute ATT or nil."
  (let ((colon (xmltokf-attribute-name-colon att)))
    (buffer-substring-no-properties
     (if colon
	 (1+ colon)
       (xmltokf-attribute-name-start att))
     (xmltokf-attribute-name-end att))))

(defun xmltokf-attribute-full-name (att)
  "Return the full name for attribute ATT.

- <a href=\"soup\"/> => \"href\".
- <a super:pointer=\"soup\"/> => \"super:pointer\"."
  (buffer-substring-no-properties
   (xmltokf-attribute-name-start att)
   (xmltokf-attribute-name-end att)))


;; (ert "test-xmltokf-attribute-full-name")

(defun xmltokf-attribute-value (att)
  "Return the value of attribute ATT."
  (let ((rnv (xmltokf-attribute-raw-normalized-value att)))
    (and rnv
	 (if (stringp rnv)
	     rnv
	   (buffer-substring-no-properties
            (xmltokf-attribute-value-start att)
	    (xmltokf-attribute-value-end att))))))


;; (ert "test-xmltokf-attribute-value")

(defun xmltokf-get-attributes-and-vals (&optional pom-or-token)
  "Return an alist of keys and values for the attributes of POM-OR-TOKEN.

POM-OR-TOKEN should be a position, marker, or ‘xmltokf-token’."
  (seq-map
   (lambda (attr)
     (cons
      (xmltokf-attribute-full-name attr)
      (xmltokf-attribute-value attr)))
   (xmltokf-token-attributes
    (if  (xmltokf-token-p pom-or-token)
        pom-or-token
      (xmltokf-scan-here pom-or-token)))))



(defun xmltokf-scan-element (pom-or-token &optional buff)
  "Get element starting at POM-OR-TOKEN as an ‘xmltokf-element’.

POM-OR-TOKEN is a marker, position, or token.

The default for buffer BUFF is the ‘current-buffer’.

This returns the results of ‘xmltokf-scan-here’ on the thing at
POSITION, and, if useful, its end.  It is only meaningful if an
empty-element, data, space, or start-tag starts at POSITION."
  (with-current-buffer (or (and (markerp pom-or-token)
                                (marker-buffer pom-or-token))
                           (and (xmltokf-token-p pom-or-token)
                                (xmltokf-token-buffer pom-or-token))
                           buff
                           (current-buffer))
    (let ((start-token (cond
                        ((xmltokf-token-p pom-or-token)
                         pom-or-token)
                        ((or (markerp pom-or-token)
                             (integerp pom-or-token))
                         (xmltokf-scan-here pom-or-token))))
	  stack)
      (cond
       ((member (xmltokf-token-type start-token) '(data space empty-element))
        (make-xmltokf-element
         :type (xmltokf-token-type start-token)
         :start (xmltokf-token-start start-token)
         :end (xmltokf-token-end start-token)
         :start-token start-token))
       ((eq (xmltokf-token-type start-token) 'start-tag)
        (push start-token stack)
        (save-excursion
	  (goto-char (xmltokf-token-end start-token))
	  (let ((next-token (xmltokf-scan-here (point))))
            (while stack
	      (when (eobp)
                (error
                 "Whoops, no closure to element starting with %s"
                 start-token))
	      (cond ((eq (xmltokf-token-type next-token) 'end-tag)
                     (pop stack))
		    ((eq (xmltokf-token-type next-token) 'start-tag)
		     (push next-token stack))
		    (t nil))
              (when stack
                (setq next-token
                      (xmltokf-scan-here
                       (xmltokf-token-end next-token)))))
            ;; next-token should be the last one
            (make-xmltokf-element
             :type 'element
             :start (xmltokf-token-start start-token)
             :end (xmltokf-token-end next-token)
             :start-token start-token
             :end-token next-token))))
       (t (error "No element starting at %s" pom-or-token))))))

;; (with-temp-buffer
;;   (insert "<soup type=\"very tasty\">a<a></a></soup>")
;;   (xmltokf-scan-element (point-min)))



;; (ert "test-xmltokf-scan-element")

(defun xmltokf-attribute-by-name (name scan-data &optional local)
  "Return attribute by NAME in SCAN-DATA.

If attribute does not have that NAME, returns nil.  If it does,
returns its (numeric) attribute values, like ‘xmltokf-attribute’.

If LOCAL is not nil, just match on local name."
  (unless (member (xmltokf-token-type scan-data) '(start-tag empty-element))
    (error "This is a %s" (xmltokf-token-type scan-data)))
  (car
   (delq nil
	 (mapcar
	  (lambda (att)
	    (when (if local
		      (string=
                       (xmltokf-attribute-local-name att) name)
		    (string=
                     (xmltokf-attribute-full-name att) name))
	      att))
	  (xmltokf-token-attributes scan-data)))))


;; (ert 'xmltokf-test-attribute-by-name)

;;; Check for types

(defun xmltokf-is-cdata-section (token)
  "Return t if TOKEN is a cdata-section."
  (eq (xmltokf-token-type token) 'cdata-section))

(defun xmltokf-is-comment (token)
  "Return t if TOKEN is a comment."
  (eq (xmltokf-token-type token) 'comment))

(defun xmltokf-is-data (token)
  "Return t if TOKEN is data."
  (eq (xmltokf-token-type token) 'data))

(defun xmltokf-is-empty-element (token)
  "Return t if TOKEN is an element."
  (eq (xmltokf-token-type token) 'empty-element))

(defun xmltokf-is-end-tag (token)
  "Return t if TOKEN is a tag."
  (eq (xmltokf-token-type token) 'end-tag))

(defun xmltokf-is-not-well-formed (token)
  "Return t if TOKEN is not well-formed."
  (eq (xmltokf-token-type token) 'not-well-formed))

(defun xmltokf-is-partial-empty-element (token)
  "Return t if TOKEN is a partial empty element."
  (eq (xmltokf-token-type token) 'partial-empty-element))

(defun xmltokf-is-partial-end-tag (token)
  "Return t if TOKEN is a partial end tag."
  (eq (xmltokf-token-type token) 'partial-end-tag))

(defun xmltokf-is-partial-start-tag (token)
  "Return t if TOKEN is a partial start tag."
  (eq (xmltokf-token-type token) 'partial-start-tag))

(defun xmltokf-is-space (token)
  "Return t if TOKEN is only space."
  (eq (xmltokf-token-type token) 'space ))

(defun xmltokf-is-start-tag (token)
  "Return t if TOKEN is a start tag."
  (eq (xmltokf-token-type token) 'start-tag))



;; (ert "test-xmltokf-is-start-tag")


(defun xmltokf-is-prolog (token)
  "Return t if TOKEN is prolog."
  (eq (xmltokf-token-type token) 'prolog))

;;; A dumb serializer

(defun xmltokf-attribute-val-to-text (attribute value)
  "Try to serialize ATTRIBUTE with VALUE."
  (string-join
   `(,(string-trim (format "%s" attribute))
     "=\""
     ,(string-trim
       (string-join
        (split-string
         (format "%s" value)
         "\"")
        "&quot;"))
     "\"")))


;; (ert "test-xmltokf-attribute-val-to-text")

;; (xmltokf-attribute-val-to-text "key" "#potato")
;; (xmltokf-attribute-val-to-text "key" "#pot\"ato")

(defun xmltokf-get-token-string (token)
  "Get the string encoding TOKEN."
  (buffer-substring-no-properties
   (xmltokf-token-start token)
   (xmltokf-token-end token)))


;; (ert "test-xmltokf-get-token-string")

(defun xmltokf-get-element-string (balanced-thing)
  "Get the string encoding BALANCED-THING.

BALANCED-THING should be as returned from ‘xmltokf-scan-element’."
  (buffer-substring-no-properties
   (xmltokf-element-start balanced-thing)
   (xmltokf-element-end balanced-thing)))


;; (ert "test-xmltokf-get-element-string")

(defun xmltokf-element-to-node (balanced-thing)
  "Parse BALANCED-THING to a node (suitable for ‘dom’ functions).

BALANCED-THING should be as returned from ‘xmltokf-scan-element’.

No namespace support! It would be very iffy, since it’s hard to
tell for elements out of their context (TODO)."
  (with-current-buffer (xmltokf-token-buffer (xmltokf-element-start-token balanced-thing))
    (car
     (xml-parse-region
      (xmltokf-element-start balanced-thing)
      (xmltokf-element-end balanced-thing)))))

(defun xmltokf-element-text-content (balanced-thing &optional separator)
  "Get text in BALANCED-THING.

BALANCED-THING should be as returned from ‘xmltokf-scan-element’.
SEPARATOR is passed as is to ‘dom-texts’."
  (dom-texts (xmltokf-element-to-node balanced-thing) separator))

;;; Some utilities for manipulating the buffer content (so much for
;;; the “functional” part).  Functions here should generally return
;;; the new scan-data for the token that they changed.

(defun xmltokf-set-attribute! (token attribute value)
  "Set TOKEN’s ATTRIBUTE to VALUE (adding it if it’s not there yet)."
  (interactive
   (list
    (or (xmltokf-scan-here (point))
	(error "Not at the beginning of a token!"))
    (read-string "Attribute to add: ")
    (read-string "Value for attribute: ")))
  (unless (or (xmltokf-is-start-tag token)
              (xmltokf-is-empty-element token))
    (error "Not something with attributes: %s"
           (xmltokf-token-type token)))
  (let ((new-attribute (xmltokf-attribute-val-to-text attribute value))
        (current-att (xmltokf-attribute-by-name attribute token)))
    (atomic-change-group
      (save-excursion
        (save-match-data
          (cond
           (current-att
            (goto-char (xmltokf-attribute-name-start current-att))
            (delete-region
             (1- (xmltokf-attribute-name-start current-att))
             (1+ (xmltokf-attribute-value-end current-att))))
           (t (goto-char (xmltokf-token-name-end token))))
          (insert " ")
          (insert new-attribute)
          (xmltokf-scan-here (xmltokf-token-start token)))))))


;; (ert "test-xmltokf-set-attribute!")


(defun xmltokf-rename-token! (token name)
  "Rename TOKEN to NAME, and return new scan-data."
  (unless (member (xmltokf-token-type token) '(empty-element start-tag end-tag))
    (error "Cannot rename token of type %s: %s" (xmltokf-token-type token) token))
  (save-excursion
    (goto-char (xmltokf-token-name-start token))
    (delete-and-extract-region
     (point)
     (xmltokf-token-name-end token))
    (insert (format "%s" name))
    (xmltokf-scan-here (xmltokf-token-start token))))

;; (with-temp-buffer
;;     (insert "<a/>")
;;     (xmltokf-rename-token! (xmltokf-scan-here (point-min)) "pot"))
;; '(xmltokf-token empty-element 1 nil 2 5 nil nil nil nil 7 #<killed buffer>)

;; (with-temp-buffer
;;   (insert "<soup type=\"very tasty\">a<a></a></soup>")
;;   (xmltokf-rename-token! (xmltokf-scan-here 33) "hello")
;;   (buffer-string))


;; (ert "test-xmltokf-rename-token!")


(defun xmltokf-rename-element! (pom-or-token name)
  "Rename the element starting at point-or-marker-or-token POM-OR-TOKEN to NAME.

Returns scan data for the new start tag."
  (with-current-buffer (cond
                        ((xmltokf-token-p pom-or-token)
                         (xmltokf-token-buffer pom-or-token))
                        ((markerp pom-or-token)
                         (marker-buffer pom-or-token))
                        (t (current-buffer)))
    (let ((item (xmltokf-scan-element pom-or-token)))
      (atomic-change-group
        (cond
         ((null item)
          (warn (format "No balanced item at position %s" pom-or-token))
          nil)
         ((xmltokf-element-end-token item)
          ;; First the end!
          (xmltokf-rename-token! (xmltokf-element-end-token item) name)
          (xmltokf-rename-token! (xmltokf-element-start-token item) name))
         ((xmltokf-element-start-token item)
          (xmltokf-rename-token! (xmltokf-element-start-token item) name))
         (t
          (warn (format "No changes for thing at position %s: %s" pom-or-token (car item)))
          nil))))))


;; (ert "test-xmltokf-rename-element!")

(defun xmltokf-drop-token! (token)
  "Remove TOKEN from buffer.

Returns the string that the token was encoded as."
  (atomic-change-group
    (if (equal token (xmltokf-scan-here (xmltokf-token-start token)))
        (delete-and-extract-region
         (xmltokf-token-start token)
         (xmltokf-token-end token))
      (error
       "Encoded token changed: found %s, expected: %s"
       (xmltokf-scan-here  (xmltokf-token-start token))
       token))))


;; (ert "test-xmltokf-drop-token!")

(defun xmltokf-drop-element! (element &optional skip-rescan)
  "Remove ELEMENT from buffer.

ELEMENT should be as returned from ‘xmltokf-scan-element’

If SKIP-RESCAN is not nil, then don’t check before removing the
string.

Returns the string that the element was encoded as."
  (unless skip-rescan
    (cl-assert
     (equal element
            (xmltokf-scan-element
             (xmltokf-token-start
              (xmltokf-element-start-token element))))
     'show-args
     "Encoded element changed: found %s, expected: %s"
     (xmltokf-scan-element
      (xmltokf-element-start element))
     element))
  (atomic-change-group
    (delete-and-extract-region
     (xmltokf-element-start element)
     (xmltokf-element-end element))))

;; (let ((s "<soup type=\"very tasty\">hello<a></a></soup>"))
;;     (with-temp-buffer
;;       (insert s)
;;       (message (buffer-string))
;;       (xmltokf-drop-element! (xmltokf-scan-element (point-min)))
;;       (message (buffer-string))))


;; (ert "test-xmltokf-drop-element!")





;;; Probably useless, given we’re usually working in a buffer
;;; containing the xml:

;; (defun xmltokf-token-to-text (token)
;;   "Return TOKEN serialized to text."
;;   (cond
;;    ((or (xmltokf-is-start-tag token)
;;         (xmltokf-is-empty-element token))
;;     (string-join
;;      `("<"
;;        ;; the name
;;        ,(xmltokf-tag-qname token)
;;        ;; attributes
;;        ,@(let ((atts-encoded
;;                 (mapcar
;;                  (lambda (att)
;;                    (xmltokf-attribute-val-to-text
;;                     (xmltokf-attribute-full-name att)
;;                     (xmltokf-attribute-value att)))
;;                  (xmltokf-token-attributes token))))
;;            (if atts-encoded
;;                `(" " ,(string-join atts-encoded " "))
;;              (list "")))
;;        ,(if (xmltokf-is-empty-element token)
;;             "/"
;;           "")
;;        ">")))
;;    (t
;;     (error "Can’t turn this into text (yet): %s" token))))

;; (with-temp-buffer
;;   (insert "<a xml:id=\"soup123\"/>")
;;   (xmltokf-token-to-text (xmltokf-scan-here (point-min))))

;; (ert-deftest test-xmltokf-token-to-text ()
;;   (let ((cases
;;          '(("<a xml:id=\"soup123\"></a>" . "<a xml:id=\"soup123\">")
;;            ("<a xml:id=\"soup123\"/>" . "<a xml:id=\"soup123\"/>")
;;            ("<a xml:id=\"soup123\" href=\"soup.html\">hello there!</a>" .
;;             "<a xml:id=\"soup123\" href=\"soup.html\">"))))
;;     (mapc
;;      (lambda (c)
;;        (with-temp-buffer
;;          (insert (car c))
;;          (should
;;           (equal
;;            (xmltokf-token-to-text (xmltokf-scan-here (point-min)))
;;            (cdr c)))))
;;      cases)))

;; (ert "test-xmltokf-token-to-text")

(provide 'xmltokf)
;;; xmltokf.el ends here
