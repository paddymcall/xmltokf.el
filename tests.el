(require 'ert)

(ert-deftest test-xmltokf-scan-here ()
  (with-temp-buffer
    (insert "<a/>")
    (should
     (equal
      (xmltokf-scan-here (point-min))
      `(xmltokf-token empty-element 1 nil 2 3 nil nil nil nil 5 ,(current-buffer)))))
  (with-temp-buffer
    (insert "</html:a>")
    (should
     (equal
      (xmltokf-scan-here (point-min))
      `(xmltokf-token end-tag 1 7 8 9 nil nil nil nil 10 ,(current-buffer))))
    (should
     (equal
      (buffer-substring-no-properties
       (xmltokf-token-name-start (xmltokf-scan-here (point-min)))
       (xmltokf-token-name-end (xmltokf-scan-here (point-min))))
      "a")))
  (with-temp-buffer
    (insert "<abcd>hello</abcd>")
    (should
     (equal
      (xmltokf-scan-here (point-min))
      `(xmltokf-token start-tag 1 nil 2 6 nil nil nil nil 7 ,(current-buffer)))))
  (with-temp-buffer
    (insert "<abcd type=\"green\">")
    (should
     (equal
      (xmltokf-scan-here (point-min))
      `(xmltokf-token start-tag 1 nil 2 6 nil
                      ([7 nil 11 13 18 t nil]) nil nil 20 ,(current-buffer)))))
  (with-temp-buffer
    (insert "beep")
    (should
     (equal
      (xmltokf-scan-here (point-min))
      `(xmltokf-token data 1 nil nil nil nil nil nil nil 5 ,(current-buffer)))))
  (with-temp-buffer
    (insert "</beep>")
    (should
     (equal
      (xmltokf-scan-here (set-marker (make-marker) (point-min)))
      `(xmltokf-token end-tag 1 nil 3 7 nil nil nil nil 8 ,(current-buffer)))))
  (with-temp-buffer
    (insert "</beep>")
    (should
     (equal
      (xmltokf-scan-here (xmltokf-scan-here (point-min)))
      `(xmltokf-token end-tag 1 nil 3 7 nil nil nil nil 8 ,(current-buffer)))))
  (with-temp-buffer
    (insert "&gt;")
    (should
     ;; TODO: different result in interactive use:
     (or
      (equal
       (xmltokf-scan-here (point-min))
       `(xmltokf-token entity-ref 1 nil nil nil ">" nil nil nil 5 ,(current-buffer)))
      (equal
       (xmltokf-scan-here (point-min))
       `(xmltokf-token entity-ref 1 nil nil nil nil nil nil
                       (["Referenced entity has not been defined" 2 4])
                       5 ,(current-buffer))
       ))))
  ;; These require  xmltok-forward-prolog, which still needs to be implemented.
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    (should
     (equal
      `(xmltokf-token processing-instruction 1 nil 2 6 nil nil nil (["Processing instruction target is xml" 3 6]) 39 ,(current-buffer))
      (xmltokf-scan-here (point-min)))))
  (with-temp-buffer
    (insert "<!DOCTYPE author [
  <!ELEMENT author (#PCDATA)>
  <!ENTITY js \"Jo Smith\">
]>")
    (should-error
     (xmltokf-scan-here (point-min))))
  (with-temp-buffer
    (insert "<!ELEMENT author (#PCDATA)>")
    (should-error
     (xmltokf-scan-here (point-min))))
  (with-temp-buffer
    (insert "<a/>")
    (should
     (equal
      (xmltokf-scan-here (point-max))
      nil))))

(ert-deftest test-xmltokf-tag-qname ()
  (with-temp-buffer
    (insert "</beep:soup>")
    (should
     (equal
      (xmltokf-tag-qname (xmltokf-scan-here (point-min)))
      "beep:soup")))
  (with-temp-buffer
    (insert "</soup>")
    (should
     (equal
      (xmltokf-tag-qname (xmltokf-scan-here (point-min)))
      "soup")))
  (with-temp-buffer
    (insert "<soup xmlns=\"http://potatos\">")
    (should
     (equal
      (xmltokf-tag-qname (xmltokf-scan-here (point-min)))
      "soup")))
  (with-temp-buffer
    (insert "<?pi1  p1 content ?>")
    (should
     (equal
      (xmltokf-tag-qname (xmltokf-scan-here (point-min)))
      "pi1"))))

(ert-deftest test-xmltokf-tag-prefix ()
  (with-temp-buffer
    (insert "<html:p>")
    (should
     (equal
      (xmltokf-tag-prefix
       (xmltokf-scan-here (point-min)))
      "html")))
  (with-temp-buffer
    (insert "<p>")
    (should
     (equal
      (xmltokf-tag-prefix
       (xmltokf-scan-here (point-min)))
      nil)))
  (with-temp-buffer
    (insert "</html:p>")
    (should
     (equal
      (xmltokf-tag-prefix
       (xmltokf-scan-here (point-min)))
      "html")))
  (with-temp-buffer
    (insert "</p>")
    (should
     (equal
      (xmltokf-tag-prefix
       (xmltokf-scan-here (point-min)))
      nil)))
  (with-temp-buffer
    (insert "nothing here!")
    (should
     (equal
      (xmltokf-tag-prefix
       (xmltokf-scan-here (point-min)))
      nil))))

(ert-deftest test-xmltokf-tag-local-name ()
  (with-temp-buffer
    (insert "</beep:soup>")
    (should
     (equal
      (xmltokf-tag-local-name (xmltokf-scan-here (point-min)))
      "soup")))
  (with-temp-buffer
    (insert "</soup>")
    (should
     (equal
      (xmltokf-tag-local-name (xmltokf-scan-here (point-min)))
      "soup")))
  (with-temp-buffer
    (insert "beep")
    (should
     (equal
      (xmltokf-tag-local-name (xmltokf-scan-here (point-min)))
      nil))))

(ert-deftest test-xmltokf-attribute-full-name ()
  (should
   (equal
    (with-temp-buffer
      (insert "<a href=\\\"soup\\\"/>")
      (xmltokf-attribute-full-name
       (xmltokf-attribute-by-name 'href (xmltokf-scan-here (point-min)))))
    "href"))
  (should
   (equal
    (with-temp-buffer
      (insert "<a super:pointer=\"soup\"/>")
      (xmltokf-attribute-full-name
       (xmltokf-attribute-by-name
        'super:pointer
        (xmltokf-scan-here (point-min)))))
    "super:pointer")))

(ert-deftest test-xmltokf-attribute-value ()
  (with-temp-buffer
    (insert "<soup type=\"very tasty\">")
    (should
     (equal
      (xmltokf-attribute-value
       (car (xmltokf-token-attributes (xmltokf-scan-here (point-min)))))
      "very tasty")))
  ;; This replaces irregular spaces/newlines in attribute values:
  (with-temp-buffer
    (insert
     (string-join
      '("<soup type=\"very"
        "\t"
        "\t"
        "\n"
        " tasty\">")
      ""))
    (should
     (equal
      (xmltokf-attribute-value
       (car
        (xmltokf-token-attributes (xmltokf-scan-here (point-min)))))
      (string-join
       '("very"
         " "
         " "
         " "
         " tasty")
       "")))))

(ert-deftest test-xmltokf-get-attributes-and-vals ()
  (with-temp-buffer
    (insert "<pb ed=\"#pva_ms\" xml:id=\"ms_105a\" n=\"105a\"/>")
    (should
     (equal
      '(("ed" . "#pva_ms")
	("xml:id" . "ms_105a")
	("n" . "105a"))
      (xmltokf-get-attributes-and-vals (point-min)))))
  (with-temp-buffer
    (insert "<pb ed=\"#pva_ms\" xml:id=\"ms_105a\" n=\"105a\"/>")
    (should
     (equal
      '(("ed" . "#pva_ms")
	("xml:id" . "ms_105a")
	("n" . "105a"))
      (let ((tok (xmltokf-scan-here (point-min))))
        (xmltokf-get-attributes-and-vals tok))))))

(ert-deftest test-xmltokf-scan-element ()
  (let ((doc "<div>
<beep/>
<beep>hello</beep>
</div>"))
    (with-temp-buffer
      (insert doc)
      (should
       (equal
	(xmltokf-scan-element (point-min))
        `(xmltokf-element
          element 1 40
          (xmltokf-token
           start-tag 1 nil 2 5 nil nil nil nil 6 ,(current-buffer))
          (xmltokf-token
           end-tag 34 nil 36 39 nil nil nil nil 40 ,(current-buffer)))))
      (should
       (equal
	(xmltokf-scan-element 7)
        `(xmltokf-element
          empty-element 7 14
          (xmltokf-token
           empty-element 7 nil 8 12 nil nil nil nil 14 ,(current-buffer))
          nil)))
      (should
       (equal
	(xmltokf-scan-element 21)
        `(xmltokf-element data 21 26
                          (xmltokf-token
                           data 21 nil nil nil nil nil nil nil 26 ,(current-buffer))
                          nil))))))

(ert-deftest xmltokf-test-attribute-by-name ()
  "See what happens when getting an attribute name."
  (with-temp-buffer
    (insert "<hello target=\"soup\" na:xml=\"beep\" xmlns:na=\"http://somewhere\" />")
    (let ((el (xmltokf-scan-here (point-min))))
      (should
       (equal (xmltokf-attribute-by-name "target" el)
	      [8 nil 14 16 20 t nil]))
      (should
       (equal (xmltokf-attribute-by-name "na:xml" el)
	      [22 24 28 30 34 t nil]))
      (should
       (equal (xmltokf-attribute-by-name "xml" el t)
	      [22 24 28 30 34 t nil])))))

(ert-deftest test-xmltokf-is-start-tag ()
  (with-temp-buffer
    (insert "<soup type=\"very tasty\">hello</soup>")
    (should
     (xmltokf-is-start-tag (xmltokf-scan-here (point-min)))))
  (with-temp-buffer
    (insert "<soup type=\"very tasty\">hello</soup>")
    (should-not
     (xmltokf-is-start-tag (xmltokf-scan-here
                            (xmltokf-token-end
                             (xmltokf-scan-here (point-min))))))))

(ert-deftest test-xmltokf-attribute-val-to-text ()
  (let ((cases
         '((("key" "#potato") .
            "key=\"#potato\"")
           (("key" "#pot\"ato") .
            "key=\"#pot&quot;ato\""))))
    (mapc
     (lambda (c)
       (should
        (equal
         (apply 'xmltokf-attribute-val-to-text (car c))
         (cdr c))))
     cases)))

(ert-deftest test-xmltokf-get-token-string ()
  (let ((s "<soup type=\"very tasty\">hello</soup>"))
    (with-temp-buffer
      (insert s)
      (should
       (equal
        (xmltokf-get-token-string
         (xmltokf-scan-here (point-min)))
        "<soup type=\"very tasty\">"))
      (should
       (equal
        (xmltokf-get-token-string
         (xmltokf-scan-here (xmltokf-token-end (xmltokf-scan-here (point-min)))))
        "hello")))))

(ert-deftest test-xmltokf-get-element-string ()
  (let ((s "<soup type=\"very tasty\">hello</soup>"))
    (with-temp-buffer
      (insert s)
      (should
       (equal
        (xmltokf-get-element-string
         (xmltokf-scan-element (point-min)))
        s))
      (should
       (equal
        (xmltokf-get-element-string
         (xmltokf-scan-element (xmltokf-token-end (xmltokf-scan-here (point-min)))))
        "hello")))))

(ert-deftest test-xmltokf-set-attribute! ()
  (with-temp-buffer
    (insert "<lb/>")
    (should
     (equal
      (xmltokf-set-attribute!
       (xmltokf-scan-here (point-min))
       "break"
       "no")
      `(xmltokf-token empty-element 1 nil 2 4 nil
                      ([5 nil 10 12 14 t nil])
                      nil nil 17
                      ,(current-buffer))))
    (should
     (equal
      (buffer-string)
      "<lb break=\"no\"/>")))
  (with-temp-buffer
    (insert "<lb   \n/>")
    (should
     (equal
      (xmltokf-set-attribute!
       (xmltokf-scan-here (point-min))
       "break"
       "no")
      `(xmltokf-token empty-element 1 nil 2 4 nil
                      ([5 nil 10 12 14 t nil])
                      nil nil 21 ,(current-buffer))))
    (should
     (equal
      (buffer-string)
      "<lb break=\"no\"   \n/>")))
  (with-temp-buffer
    (insert "<lb   break=\"yes\"/>")
    (should
     (equal
      (xmltokf-set-attribute!
       (xmltokf-scan-here (point-min))
       "break"
       "no")
      `(xmltokf-token empty-element 1 nil 2 4 nil
                      ([7 nil 12 14 16 t nil])
                      nil nil 19 ,(current-buffer))))
    (should
     (equal
      (buffer-string)
      "<lb   break=\"no\"/>")))
  (with-temp-buffer
    (insert "<p type=\"long\">...</p>")
    (should
     (equal
      (xmltokf-set-attribute!
       (xmltokf-scan-here (point-min))
       "break"
       "no")
      `(xmltokf-token start-tag 1 nil 2 3 nil
                      ([4 nil 9 11 13 t nil]
                       [15 nil 19 21 25 t nil])
                      nil nil 27 ,(current-buffer))))
    (should
     (equal
      (buffer-string)
      "<p break=\"no\" type=\"long\">...</p>")))
  (with-temp-buffer
    (insert "<p type=\"long\">hello</p>")
    (should
     (equal
      (xmltokf-set-attribute!
       (xmltokf-scan-here (point-min))
       "type"
       "short")
      `(xmltokf-token start-tag 1 nil 2 3 nil
                      ([4 nil 8 10 15 t nil])
                      nil nil 17 ,(current-buffer))))
    (should
     (equal
      (buffer-string)
      "<p type=\"short\">hello</p>"))))

(ert-deftest test-xmltokf-rename-token! ()
  (with-temp-buffer
    (insert "<a/>")
    (should
     (equal
      (xmltokf-rename-token! (xmltokf-scan-here (point-min)) "pot")
      `(xmltokf-token empty-element 1 nil 2 5 nil nil nil nil 7 ,(current-buffer))))
    (should (equal (buffer-string) "<pot/>")))
  (with-temp-buffer
    (insert "<a   />")
    (should
     (equal
      (xmltokf-rename-token! (xmltokf-scan-here (point-min)) "pot")
      `(xmltokf-token empty-element 1 nil 2 5 nil nil nil nil 10 ,(current-buffer))))
    (should (equal (buffer-string) "<pot   />")))
  (with-temp-buffer
    (insert "<a xmlns=\"great\" type=\"green\"> hello there! </a>")
    (should
     (equal
      (xmltokf-rename-token! (xmltokf-scan-here (point-min)) "pot")
      `(xmltokf-token start-tag 1 nil 2 5 nil
                      ([20 nil 24 26 31 t nil])
                      ([6 nil 11 13 18 t nil])
                      nil 33 ,(current-buffer))))
    ;; unbalanced!
    (should
     (equal
      (buffer-string)
      "<pot xmlns=\"great\" type=\"green\"> hello there! </a>"))))

(ert-deftest test-xmltokf-rename-element! ()
  (with-temp-buffer
    (insert "<a xmlns=\"great\" type=\"green\"> hello there! </a>")
    (should
     (equal
      (xmltokf-rename-element! (xmltokf-scan-here (point-min)) "pot")
      `(xmltokf-token start-tag 1 nil 2 5 nil
                      ([20 nil 24 26 31 t nil])
                      ([6 nil 11 13 18 t nil])
                      nil 33 ,(current-buffer))))
    ;; unbalanced!
    (should
     (equal
      (buffer-string)
      "<pot xmlns=\"great\" type=\"green\"> hello there! </pot>"))))

(ert-deftest test-xmltokf-drop-token! ()
  (let ((s "<soup type=\"very tasty\"/>"))
    (with-temp-buffer
      (insert s)
      (should
       (equal
        (xmltokf-drop-token! (xmltokf-scan-here (point-min)))
        s))
      (should
       (equal
        (buffer-string)
        "")))))

(ert-deftest test-xmltokf-drop-element! ()
  (let ((s "<soup type=\"very tasty\">hello<a></a></soup>"))
    (with-temp-buffer
      (insert s)
      (xmltokf-drop-element! (xmltokf-scan-element (point-min)))
      (should
       (equal
        (buffer-string)
        "")))))

