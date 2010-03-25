;;; pisigma-input.el --- The PiSigma input method

;;; Commentary:

;; A highly customisable input method which can inherit from other
;; Quail input methods. By default the input method is geared towards
;; the input of mathematical and other symbols in PiSigma programs.
;;
;; Use M-x customize-group pisigma-input to customise this input
;; method.  Note that the functions defined under "Functions used to
;; tweak translation pairs" below can be used to tweak both the key
;; translations inherited from other input methods as well as the ones
;; added specifically for this one.
;;
;; Use pisigma-input-show-translations to see all the characters which
;; can be typed using this input method (except for those
;; corresponding to ASCII characters).

;;; Code:

(require 'quail)
(require 'cl)

;; Quail is quite stateful, so be careful when editing this code.
;; Note that with-temp-buffer is used below whenever buffer-local
;; state is modified.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun pisigma-input-concat-map (f xs)
  "Concat (map F XS)."
  (apply 'append (mapcar f xs)))

(defun pisigma-input-to-string-list (s)
  "Convert a string S to a list of one-character strings, after
removing all space and newline characters."
  (pisigma-input-concat-map
   (lambda (c) (if (member c (string-to-list " \n"))
              nil
            (list (string c))))
   (string-to-list s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used to tweak translation pairs

;; lexical-let is used since Elisp lacks lexical scoping.

(defun pisigma-input-compose (f g)
  "\x -> concatMap F (G x)"
  (lexical-let ((f1 f) (g1 g))
    (lambda (x) (pisigma-input-concat-map f1 (funcall g1 x)))))

(defun pisigma-input-or (f g)
  "\x -> F x ++ G x"
  (lexical-let ((f1 f) (g1 g))
    (lambda (x) (append (funcall f1 x) (funcall g1 x)))))

(defun pisigma-input-nonempty ()
  "Only keep pairs with a non-empty first component."
  (lambda (x) (if (> (length (car x)) 0) (list x))))

(defun pisigma-input-prepend (prefix)
  "Prepend PREFIX to all key sequences."
  (lexical-let ((prefix1 prefix))
    (lambda (x) `((,(concat prefix1 (car x)) . ,(cdr x))))))

(defun pisigma-input-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX."
  (lexical-let ((prefix1 prefix))
    (lambda (x)
      (if (equal (substring (car x) 0 (length prefix1)) prefix1)
          (list x)))))

(defun pisigma-input-suffix (suffix)
  "Only keep pairs whose key sequence ends with SUFFIX."
  (lexical-let ((suffix1 suffix))
    (lambda (x)
      (if (equal (substring (car x)
                            (- (length (car x)) (length suffix1)))
                 suffix1)
          (list x)))))

(defun pisigma-input-drop (ss)
  "Drop pairs matching one of the given key sequences.
SS should be a list of strings."
  (lexical-let ((ss1 ss))
    (lambda (x) (unless (member (car x) ss1) (list x)))))

(defun pisigma-input-drop-beginning (n)
  "Drop N characters from the beginning of each key sequence."
  (lexical-let ((n1 n))
    (lambda (x) `((,(substring (car x) n1) . ,(cdr x))))))

(defun pisigma-input-drop-end (n)
  "Drop N characters from the end of each key sequence."
  (lexical-let ((n1 n))
    (lambda (x)
      `((,(substring (car x) 0 (- (length (car x)) n1)) .
         ,(cdr x))))))

(defun pisigma-input-drop-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX.
This prefix is dropped."
  (pisigma-input-compose
   (pisigma-input-drop-beginning (length prefix))
   (pisigma-input-prefix prefix)))

(defun pisigma-input-drop-suffix (suffix)
  "Only keep pairs whose key sequence ends with SUFFIX.
This suffix is dropped."
  (lexical-let ((suffix1 suffix))
    (pisigma-input-compose
     (pisigma-input-drop-end (length suffix1))
     (pisigma-input-suffix suffix1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

;; The :set keyword is set to
;; 'pisigma-input-incorporate-changed-setting so that the input method
;; gets updated immediately when users customize it. However, the
;; setup functions cannot be run before all variables have been
;; defined. Hence the :initialize keyword is set to
;; 'custom-initialize-default to ensure that the setup is not
;; performed until pisigma-input-setup is called at the end of this
;; file.

(defgroup pisigma-input nil
  "The PiSigma input method.
After tweaking these settings you may want to inspect the
resulting translations using `pisigma-input-show-translations'."
  :group 'pisigma
  :group 'leim)

(defcustom pisigma-input-tweak-all
  '(pisigma-input-compose
    (pisigma-input-prepend "\\")
    (pisigma-input-nonempty))
  "An expression yielding a function which can be used to tweak
all translations before they are included in the input method.
The resulting function (if non-nil) is applied to every
\(KEY-SEQUENCE . TRANSLATION) pair and should return a list of
such pairs. (Note that the translations can be anything accepted
by `quail-defrule'.)

If you change this setting manually (without using the
customization buffer) you need to call `pisigma-input-setup' in
order for the change to take effect."
  :group 'pisigma-input
  :set 'pisigma-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type 'sexp)

(defcustom pisigma-input-inherit
  `(("TeX" . (pisigma-input-compose
              (pisigma-input-drop '("geq" "leq" "bullet" "qed"))
              (pisigma-input-or
               (pisigma-input-drop-prefix "\\")
               (pisigma-input-or
                (pisigma-input-compose
                 (pisigma-input-drop '("^o"))
                 (pisigma-input-prefix "^"))
                (pisigma-input-prefix "_")))))
    )
  "A list of Quail input methods whose translations should be
inherited by the PiSigma input method (with the exception of
translations corresponding to ASCII characters).

The list consists of pairs (qp . tweak), where qp is the name of
a Quail package, and tweak is an expression of the same kind as
`pisigma-input-tweak-all' which is used to tweak the translation
pairs of the input method.

The inherited translation pairs are added last, after
`pisigma-input-user-translations' and
`pisigma-input-translations'.

If you change this setting manually (without using the
customization buffer) you need to call `pisigma-input-setup' in
order for the change to take effect."
  :group 'pisigma-input
  :set 'pisigma-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Quail package")
                       (sexp :tag "Tweaking function"))))

(defcustom pisigma-input-translations
  (let ((max-lisp-eval-depth 2800)) `(

  ;; Equality and similar symbols.

  ("eq"  . ,(pisigma-input-to-string-list "=∼∽≈≋∻∾∿≀≃⋍≂≅ ≌≊≡≣≐≑≒≓≔≕≖≗≘≙≚≛≜≝≞≟≍≎≏≬⋕"))
  ("eqn" . ,(pisigma-input-to-string-list "≠≁ ≉     ≄  ≇≆  ≢                 ≭    "))

                    ("=n"  . ("≠"))
  ("~"    . ("∼"))  ("~n"  . ("≁"))
  ("~~"   . ("≈"))  ("~~n" . ("≉"))
  ("~~~"  . ("≋"))
  (":~"   . ("∻"))
  ("~-"   . ("≃"))  ("~-n" . ("≄"))
  ("-~"   . ("≂"))
  ("~="   . ("≅"))  ("~=n" . ("≇"))
  ("~~-"  . ("≊"))
  ("=="   . ("≡"))  ("==n" . ("≢"))
  ("==="  . ("≣"))
  (".="   . ("≐"))  (".=." . ("≑"))
  (":="   . ("≔"))  ("=:"  . ("≕"))
  ("=o"   . ("≗"))
  ("(="   . ("≘"))
  ("and=" . ("≙"))  ("or=" . ("≚"))
  ("*="   . ("≛"))
  ("t="   . ("≜"))
  ("def=" . ("≝"))
  ("m="   . ("≞"))
  ("?="   . ("≟"))

  ;; Inequality and similar symbols.

  ("leq"  . ,(pisigma-input-to-string-list "<≪⋘≤≦≲ ≶≺≼≾⊂⊆ ⋐⊏⊑ ⊰⊲⊴⋖⋚⋜⋞"))
  ("leqn" . ,(pisigma-input-to-string-list "≮  ≰≨≴⋦≸⊀ ⋨⊄⊈⊊  ⋢⋤ ⋪⋬   ⋠"))
  ("geq"  . ,(pisigma-input-to-string-list ">≫⋙≥≧≳ ≷≻≽≿⊃⊇ ⋑⊐⊒ ⊱⊳⊵⋗⋛⋝⋟"))
  ("geqn" . ,(pisigma-input-to-string-list "≯  ≱≩≵⋧≹⊁ ⋩⊅⊉⊋  ⋣⋥ ⋫⋭   ⋡"))

  ("<="   . ("≤"))  (">="   . ("≥"))
  ("<=n"  . ("≰"))  (">=n"  . ("≱"))
  ("len"  . ("≰"))  ("gen"  . ("≱"))
  ("<n"   . ("≮"))  (">n"   . ("≯"))
  ("<~"   . ("≲"))  (">~"   . ("≳"))
  ("<~n"  . ("⋦"))  (">~n"  . ("⋧"))
  ("<~nn" . ("≴"))  (">~nn" . ("≵"))

  ("sub"   . ("⊂"))  ("sup"   . ("⊃"))
  ("subn"  . ("⊄"))  ("supn"  . ("⊅"))
  ("sub="  . ("⊆"))  ("sup="  . ("⊇"))
  ("sub=n" . ("⊈"))  ("sup=n" . ("⊉"))

  ("squb"   . ("⊏"))  ("squp"   . ("⊐"))
  ("squb="  . ("⊑"))  ("squp="  . ("⊒"))
  ("squb=n" . ("⋢"))  ("squp=n" . ("⋣"))

  ;; Set membership etc.

  ("member" . ,(pisigma-input-to-string-list "∈∉∊∋∌∍⋲⋳⋴⋵⋶⋷⋸⋹⋺⋻⋼⋽⋾⋿"))

  ("inn" . ("∉"))
  ("nin" . ("∌"))

  ;; Intersections, unions etc.

  ("intersection" . ,(pisigma-input-to-string-list "∩⋂∧⋀⋏⨇⊓⨅⋒∏ ⊼      ⨉"))
  ("union"        . ,(pisigma-input-to-string-list "∪⋃∨⋁⋎⨈⊔⨆⋓∐⨿⊽⊻⊍⨃⊎⨄⊌∑⅀"))

  ("and" . ("∧"))  ("or"  . ("∨"))
  ("And" . ("⋀"))  ("Or"  . ("⋁"))
  ("i"   . ("∩"))  ("un"  . ("∪"))  ("u+" . ("⊎"))  ("u." . ("⊍"))
  ("I"   . ("⋂"))  ("Un"  . ("⋃"))  ("U+" . ("⨄"))  ("U." . ("⨃"))
  ("glb" . ("⊓"))  ("lub" . ("⊔"))
  ("Glb" . ("⨅"))  ("Lub" . ("⨆"))

  ;; Entailment etc.

  ("entails" . ,(pisigma-input-to-string-list "⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯"))

  ("|-"   . ("⊢"))  ("|-n"  . ("⊬"))
  ("-|"   . ("⊣"))
  ("|="   . ("⊨"))  ("|=n"  . ("⊭"))
  ("||-"  . ("⊩"))  ("||-n" . ("⊮"))
  ("||="  . ("⊫"))  ("||=n" . ("⊯"))
  ("|||-" . ("⊪"))

  ;; Divisibility, parallelity.

  ("|"  . ("∣"))  ("|n"  . ("∤"))
  ("||" . ("∥"))  ("||n" . ("∦"))

  ;; Some symbols from logic and set theory.

  ("all" . ("∀"))
  ("ex"  . ("∃"))
  ("exn" . ("∄"))
  ("0"   . ("∅"))
  ("C"   . ("∁"))

  ;; Corners, ceilings and floors.

  ("c"  . ,(pisigma-input-to-string-list "⌜⌝⌞⌟⌈⌉⌊⌋"))
  ("cu" . ,(pisigma-input-to-string-list "⌜⌝  ⌈⌉  "))
  ("cl" . ,(pisigma-input-to-string-list "  ⌞⌟  ⌊⌋"))

  ("cul" . ("⌜"))  ("cuL" . ("⌈"))
  ("cur" . ("⌝"))  ("cuR" . ("⌉"))
  ("cll" . ("⌞"))  ("clL" . ("⌊"))
  ("clr" . ("⌟"))  ("clR" . ("⌋"))

  ;; Various operators/symbols.

  ("qed"       . ("∎"))
  ("x"         . ("×"))
  ("o"         . ("∘"))
  ("comp"      . ("∘"))
  ("."         . ("∙"))
  ("*"         . ("⋆"))
  (".+"        . ("∔"))
  (".-"        . ("∸"))
  (":"         . ("∶"))
  ("::"        . ("∷"))
  ("::-"       . ("∺"))
  ("-:"        . ("∹"))
  ("+ "        . ("⊹"))
  ("surd3"     . ("∛"))
  ("surd4"     . ("∜"))
  ("increment" . ("∆"))
  ("inf"       . ("∞"))

  ;; Circled operators.

  ("o+"  . ("⊕"))
  ("o--" . ("⊖"))
  ("ox"  . ("⊗"))
  ("o/"  . ("⊘"))
  ("o."  . ("⊙"))
  ("oo"  . ("⊚"))
  ("o*"  . ("⊛"))
  ("o="  . ("⊜"))
  ("o-"  . ("⊝"))

  ("O+"  . ("⨁"))
  ("Ox"  . ("⨂"))
  ("O."  . ("⨀"))
  ("O*"  . ("⍟"))

  ;; Boxed operators.

  ("b+" . ("⊞"))
  ("b-" . ("⊟"))
  ("bx" . ("⊠"))
  ("b." . ("⊡"))

  ;; Various symbols.

  ("integral" . ,(pisigma-input-to-string-list "∫∬∭∮∯∰∱∲∳"))
  ("angle"    . ,(pisigma-input-to-string-list "∟∡∢⊾⊿"))
  ("join"     . ,(pisigma-input-to-string-list "⋈⋉⋊⋋⋌⨝⟕⟖⟗"))

  ;; Arrows.

  ("l"  . ,(pisigma-input-to-string-list "←⇐⇚⇇⇆↤⇦↞↼↽⇠⇺↜⇽⟵⟸↚⇍⇷ ↹     ↢↩↫⇋⇜⇤⟻⟽⤆↶↺⟲                                    "))
  ("r"  . ,(pisigma-input-to-string-list "→⇒⇛⇉⇄↦⇨↠⇀⇁⇢⇻↝⇾⟶⟹↛⇏⇸⇶ ↴    ↣↪↬⇌⇝⇥⟼⟾⤇↷↻⟳⇰⇴⟴⟿ ➵➸➙➔➛➜➝➞➟➠➡➢➣➤➧➨➩➪➫➬➭➮➯➱➲➳➺➻➼➽➾"))
  ("u"  . ,(pisigma-input-to-string-list "↑⇑⟰⇈⇅↥⇧↟↿↾⇡⇞          ↰↱➦ ⇪⇫⇬⇭⇮⇯                                          "))
  ("d"  . ,(pisigma-input-to-string-list "↓⇓⟱⇊⇵↧⇩↡⇃⇂⇣⇟         ↵↲↳➥ ↯                                               "))
  ("ud" . ,(pisigma-input-to-string-list "↕⇕   ↨⇳                                                                   "))
  ("lr" . ,(pisigma-input-to-string-list "↔⇔         ⇼↭⇿⟷⟺↮⇎⇹                                                       "))
  ("ul" . ,(pisigma-input-to-string-list "↖⇖                        ⇱↸                                              "))
  ("ur" . ,(pisigma-input-to-string-list "↗⇗                                         ➶➹➚                            "))
  ("dr" . ,(pisigma-input-to-string-list "↘⇘                        ⇲                ➴➷➘                            "))
  ("dl" . ,(pisigma-input-to-string-list "↙⇙                                                                        "))

  ("l-"  . ("←"))  ("<-"  . ("←"))  ("l="  . ("⇐"))
  ("r-"  . ("→"))  ("->"  . ("→"))  ("r="  . ("⇒"))  ("=>"  . ("⇒"))
  ("u-"  . ("↑"))                   ("u="  . ("⇑"))
  ("d-"  . ("↓"))                   ("d="  . ("⇓"))
  ("ud-" . ("↕"))                   ("ud=" . ("⇕"))
  ("lr-" . ("↔"))  ("<->" . ("↔"))  ("lr=" . ("⇔"))  ("<=>" . ("⇔"))
  ("ul-" . ("↖"))                   ("ul=" . ("⇖"))
  ("ur-" . ("↗"))                   ("ur=" . ("⇗"))
  ("dr-" . ("↘"))                   ("dr=" . ("⇘"))
  ("dl-" . ("↙"))                   ("dl=" . ("⇙"))

  ("l==" . ("⇚"))  ("l-2" . ("⇇"))                   ("l-r-" . ("⇆"))
  ("r==" . ("⇛"))  ("r-2" . ("⇉"))  ("r-3" . ("⇶"))  ("r-l-" . ("⇄"))
  ("u==" . ("⟰"))  ("u-2" . ("⇈"))                   ("u-d-" . ("⇅"))
  ("d==" . ("⟱"))  ("d-2" . ("⇊"))                   ("d-u-" . ("⇵"))

  ("l--"  . ("⟵"))  ("<--"  . ("⟵"))  ("l~"  . ("↜" "⇜"))
  ("r--"  . ("⟶"))  ("-->"  . ("⟶"))  ("r~"  . ("↝" "⇝" "⟿"))
  ("lr--" . ("⟷"))  ("<-->" . ("⟷"))  ("lr~" . ("↭"))

  ("l-n"  . ("↚"))  ("<-n"  . ("↚"))  ("l=n"  . ("⇍"))
  ("r-n"  . ("↛"))  ("->n"  . ("↛"))  ("r=n"  . ("⇏"))  ("=>n"  . ("⇏"))
  ("lr-n" . ("↮"))  ("<->n" . ("↮"))  ("lr=n" . ("⇎"))  ("<=>n" . ("⇎"))

  ("l-|"  . ("↤"))  ("ll-" . ("↞"))
  ("r-|"  . ("↦"))  ("rr-" . ("↠"))
  ("u-|"  . ("↥"))  ("uu-" . ("↟"))
  ("d-|"  . ("↧"))  ("dd-" . ("↡"))
  ("ud-|" . ("↨"))

  ("dz" . ("↯"))

  ;; Ellipsis.

  ("..." . ,(pisigma-input-to-string-list "⋯⋮⋰⋱"))

  ;; Box-drawing characters.

  ("---" . ,(pisigma-input-to-string-list "─│┌┐└┘├┤┬┼┴╴╵╶╷╭╮╯╰╱╲╳"))
  ("--=" . ,(pisigma-input-to-string-list "═║╔╗╚╝╠╣╦╬╩     ╒╕╘╛╞╡╤╪╧ ╓╖╙╜╟╢╥╫╨"))
  ("--_" . ,(pisigma-input-to-string-list "━┃┏┓┗┛┣┫┳╋┻╸╹╺╻
                                        ┍┯┑┕┷┙┝┿┥┎┰┒┖┸┚┠╂┨┞╀┦┟╁┧┢╈┪┡╇┩
                                        ┮┭┶┵┾┽┲┱┺┹╊╉╆╅╄╃ ╿╽╼╾"))
  ("--." . ,(pisigma-input-to-string-list "╌╎┄┆┈┊
                                        ╍╏┅┇┉┋"))

  ;; Triangles.

  ;; Big/small, black/white.

  ("t" . ,(pisigma-input-to-string-list "◂◃◄◅▸▹►▻▴▵▾▿◢◿◣◺◤◸◥◹"))
  ("T" . ,(pisigma-input-to-string-list "◀◁▶▷▲△▼▽◬◭◮"))

  ("tb" . ,(pisigma-input-to-string-list "◂▸▴▾◄►◢◣◤◥"))
  ("tw" . ,(pisigma-input-to-string-list "◃▹▵▿◅▻◿◺◸◹"))

  ("Tb" . ,(pisigma-input-to-string-list "◀▶▲▼"))
  ("Tw" . ,(pisigma-input-to-string-list "◁▷△▽"))

  ;; Squares.

  ("sq"  . ,(pisigma-input-to-string-list "■□◼◻◾◽▣▢▤▥▦▧▨▩◧◨◩◪◫◰◱◲◳"))
  ("sqb" . ,(pisigma-input-to-string-list "■◼◾"))
  ("sqw" . ,(pisigma-input-to-string-list "□◻◽"))
  ("sq." . ("▣"))
  ("sqo" . ("▢"))

  ;; Rectangles.

  ("re"  . ,(pisigma-input-to-string-list "▬▭▮▯"))
  ("reb" . ,(pisigma-input-to-string-list "▬▮"))
  ("rew" . ,(pisigma-input-to-string-list "▭▯"))

  ;; Parallelograms.

  ("pa"  . ,(pisigma-input-to-string-list "▰▱"))
  ("pab" . ("▰"))
  ("paw" . ("▱"))

  ;; Diamonds.

  ("di"  . ,(pisigma-input-to-string-list "◆◇◈"))
  ("dib" . ("◆"))
  ("diw" . ("◇"))
  ("di." . ("◈"))

  ;; Circles.

  ("ci"   . ,(pisigma-input-to-string-list "●○◎◌◯◍◐◑◒◓◔◕◖◗◠◡◴◵◶◷⚆⚇⚈⚉"))
  ("cib"  . ("●"))
  ("ciw"  . ("○"))
  ("ci."  . ("◎"))
  ("ci.." . ("◌"))
  ("ciO"  . ("◯"))

  ;; Stars.

  ("st"   . ,(pisigma-input-to-string-list "⋆✦✧✶✴✹ ★☆✪✫✯✰✵✷✸"))
  ("st4"  . ,(pisigma-input-to-string-list "✦✧"))
  ("st6"  . ("✶"))
  ("st8"  . ("✴"))
  ("st12" . ("✹"))

  ;; Blackboard bold letters.

  ("bn"   . ("ℕ"))
  ("bz"   . ("ℤ"))
  ("bq"   . ("ℚ"))
  ("br"   . ("ℝ"))
  ("bc"   . ("ℂ"))
  ("bp"   . ("ℙ"))
  ("bsum" . ("⅀"))

  ;; Parentheses.

  ("(" . ,(pisigma-input-to-string-list "([{⁅⁽₍〈⎴⟦⟨⟪〈《「『【〔〖〚︵︷︹︻︽︿﹁﹃﹙﹛﹝（［｛｢"))
  (")" . ,(pisigma-input-to-string-list ")]}⁆⁾₎〉⎵⟧⟩⟫〉》」』】〕〗〛︶︸︺︼︾﹀﹂﹄﹚﹜﹞）］｝｣"))

  ("[[" . ("⟦"))
  ("]]" . ("⟧"))
  ("<"  . ("⟨"))
  (">"  . ("⟩"))
  ("<<" . ("⟪"))
  (">>" . ("⟫"))

  ;; Primes.

  ("'" . ,(pisigma-input-to-string-list "′″‴⁗"))
  ("`" . ,(pisigma-input-to-string-list "‵‶‷"))

  ;; Fractions.

  ("frac" . ,(pisigma-input-to-string-list "¼½¾⅓⅔⅕⅖⅗⅘⅙⅚⅛⅜⅝⅞⅟"))

  ;; Bullets.

  ("bu"  . ,(pisigma-input-to-string-list "•◦‣⁌⁍"))
  ("bub" . ("•"))
  ("buw" . ("◦"))
  ("but" . ("‣"))

  ;; Musical symbols.

  ("note" . ,(pisigma-input-to-string-list "♩♪♫♬"))
  ("b"    . ("♭"))
  ("#"    . ("♯"))

  ;; Other punctuation and symbols.

  ("\\"         . ("\\"))
  ("en"         . ("–"))
  ("em"         . ("—"))
  ("^i"         . ("ⁱ"))
  ("!!"         . ("‼"))
  ("??"         . ("⁇"))
  ("?!"         . ("‽" "⁈"))
  ("!?"         . ("⁉"))
  ("die"        . ,(pisigma-input-to-string-list "⚀⚁⚂⚃⚄⚅"))
  ("asterisk"   . ,(pisigma-input-to-string-list "⁎⁑⁂✢✣✤✥✱✲✳✺✻✼✽❃❉❊❋"))
  ("8<"         . ("✂" "✄"))
  ("tie"        . ("⁀"))
  ("undertie"   . ("‿"))
  ("apl"        . ,(pisigma-input-to-string-list "⌶⌷⌸⌹⌺⌻⌼⌽⌾⌿⍀⍁⍂⍃⍄⍅⍆⍇⍈
                                               ⍉⍊⍋⍌⍍⍎⍏⍐⍑⍒⍓⍔⍕⍖⍗⍘⍙⍚⍛
                                               ⍜⍝⍞⍟⍠⍡⍢⍣⍤⍥⍦⍧⍨⍩⍪⍫⍬⍭⍮
                                               ⍯⍰⍱⍲⍳⍴⍵⍶⍷⍸⍹⍺⎕"))

  ;; Shorter forms of many greek letters plus ƛ.

  ("Ga"  . ("α"))  ("GA"  . ("Α"))
  ("Gb"  . ("β"))  ("GB"  . ("Β"))
  ("Gg"  . ("γ"))  ("GG"  . ("Γ"))
  ("Gd"  . ("δ"))  ("GD"  . ("Δ"))
  ("Ge"  . ("ε"))  ("GE"  . ("Ε"))
  ("Gz"  . ("ζ"))  ("GZ"  . ("Ζ"))
  ;; \eta \Eta
  ("Gth" . ("θ"))  ("GTH" . ("θ"))
  ("Gi"  . ("ι"))  ("GI"  . ("Ι"))
  ("Gk"  . ("κ"))  ("GK"  . ("Κ"))
  ("Gl"  . ("λ"))  ("GL"  . ("Λ"))  ("Gl-" . ("ƛ"))
  ("Gm"  . ("μ"))  ("GM"  . ("Μ"))
  ("Gn"  . ("ν"))  ("GN"  . ("Ν"))
  ("Gx"  . ("ξ"))  ("GX"  . ("Ξ"))
  ;; \omicron \Omicron
  ;; \pi \Pi
  ("Gr"  . ("ρ"))  ("GR"  . ("Ρ"))
  ("Gs"  . ("σ"))  ("GS"  . ("Σ"))
  ("Gt"  . ("τ"))  ("GT"  . ("Τ"))
  ("Gu"  . ("υ"))  ("GU"  . ("Υ"))
  ("Gf"  . ("φ"))  ("GF"  . ("Φ"))
  ("Gc"  . ("χ"))  ("GC"  . ("Χ"))
  ("Gp"  . ("ψ"))  ("GP"  . ("Ψ"))
  ("Go"  . ("ω"))  ("GO"  . ("Ω"))

  ;; Some ISO8859-1 characters.

  (" "         . (" "))
  ("!"         . ("¡"))
  ("cent"      . ("¢"))
  ("brokenbar" . ("¦"))
  ("degree"    . ("°"))
  ("?"         . ("¿"))
  ("^a_"       . ("ª"))
  ("^o_"       . ("º"))

  ;; Circled, parenthesised etc. numbers and letters.

  ( "(0)" . ,(pisigma-input-to-string-list " ⓪"))
  ( "(1)" . ,(pisigma-input-to-string-list "⑴①⒈❶➀➊"))
  ( "(2)" . ,(pisigma-input-to-string-list "⑵②⒉❷➁➋"))
  ( "(3)" . ,(pisigma-input-to-string-list "⑶③⒊❸➂➌"))
  ( "(4)" . ,(pisigma-input-to-string-list "⑷④⒋❹➃➍"))
  ( "(5)" . ,(pisigma-input-to-string-list "⑸⑤⒌❺➄➎"))
  ( "(6)" . ,(pisigma-input-to-string-list "⑹⑥⒍❻➅➏"))
  ( "(7)" . ,(pisigma-input-to-string-list "⑺⑦⒎❼➆➐"))
  ( "(8)" . ,(pisigma-input-to-string-list "⑻⑧⒏❽➇➑"))
  ( "(9)" . ,(pisigma-input-to-string-list "⑼⑨⒐❾➈➒"))
  ("(10)" . ,(pisigma-input-to-string-list "⑽⑩⒑❿➉➓"))
  ("(11)" . ,(pisigma-input-to-string-list "⑾⑪⒒"))
  ("(12)" . ,(pisigma-input-to-string-list "⑿⑫⒓"))
  ("(13)" . ,(pisigma-input-to-string-list "⒀⑬⒔"))
  ("(14)" . ,(pisigma-input-to-string-list "⒁⑭⒕"))
  ("(15)" . ,(pisigma-input-to-string-list "⒂⑮⒖"))
  ("(16)" . ,(pisigma-input-to-string-list "⒃⑯⒗"))
  ("(17)" . ,(pisigma-input-to-string-list "⒄⑰⒘"))
  ("(18)" . ,(pisigma-input-to-string-list "⒅⑱⒙"))
  ("(19)" . ,(pisigma-input-to-string-list "⒆⑲⒚"))
  ("(20)" . ,(pisigma-input-to-string-list "⒇⑳⒛"))

  ("(a)"  . ,(pisigma-input-to-string-list "⒜Ⓐⓐ"))
  ("(b)"  . ,(pisigma-input-to-string-list "⒝Ⓑⓑ"))
  ("(c)"  . ,(pisigma-input-to-string-list "⒞Ⓒⓒ"))
  ("(d)"  . ,(pisigma-input-to-string-list "⒟Ⓓⓓ"))
  ("(e)"  . ,(pisigma-input-to-string-list "⒠Ⓔⓔ"))
  ("(f)"  . ,(pisigma-input-to-string-list "⒡Ⓕⓕ"))
  ("(g)"  . ,(pisigma-input-to-string-list "⒢Ⓖⓖ"))
  ("(h)"  . ,(pisigma-input-to-string-list "⒣Ⓗⓗ"))
  ("(i)"  . ,(pisigma-input-to-string-list "⒤Ⓘⓘ"))
  ("(j)"  . ,(pisigma-input-to-string-list "⒥Ⓙⓙ"))
  ("(k)"  . ,(pisigma-input-to-string-list "⒦Ⓚⓚ"))
  ("(l)"  . ,(pisigma-input-to-string-list "⒧Ⓛⓛ"))
  ("(m)"  . ,(pisigma-input-to-string-list "⒨Ⓜⓜ"))
  ("(n)"  . ,(pisigma-input-to-string-list "⒩Ⓝⓝ"))
  ("(o)"  . ,(pisigma-input-to-string-list "⒪Ⓞⓞ"))
  ("(p)"  . ,(pisigma-input-to-string-list "⒫Ⓟⓟ"))
  ("(q)"  . ,(pisigma-input-to-string-list "⒬Ⓠⓠ"))
  ("(r)"  . ,(pisigma-input-to-string-list "⒭Ⓡⓡ"))
  ("(s)"  . ,(pisigma-input-to-string-list "⒮Ⓢⓢ"))
  ("(t)"  . ,(pisigma-input-to-string-list "⒯Ⓣⓣ"))
  ("(u)"  . ,(pisigma-input-to-string-list "⒰Ⓤⓤ"))
  ("(v)"  . ,(pisigma-input-to-string-list "⒱Ⓥⓥ"))
  ("(w)"  . ,(pisigma-input-to-string-list "⒲Ⓦⓦ"))
  ("(x)"  . ,(pisigma-input-to-string-list "⒳Ⓧⓧ"))
  ("(y)"  . ,(pisigma-input-to-string-list "⒴Ⓨⓨ"))
  ("(z)"  . ,(pisigma-input-to-string-list "⒵Ⓩⓩ"))

  ))
  "A list of translations specific to the PiSigma input method.
Each element is a pair (KEY-SEQUENCE-STRING . LIST-OF-TRANSLATION-STRINGS).
All the translation strings are possible translations
of the given key sequence; if there is more than one you can choose
between them using the arrow keys.

Note that if you customize this setting you will not
automatically benefit (or suffer) from modifications to its
default value when the library is updated.  If you just want to
add some bindings it is probably a better idea to customize
`pisigma-input-user-translations'.

These translation pairs are included after those in
`pisigma-input-user-translations', but before the ones inherited
from other input methods (see `pisigma-input-inherit').

If you change this setting manually (without using the
customization buffer) you need to call `pisigma-input-setup' in
order for the change to take effect."
  :group 'pisigma-input
  :set 'pisigma-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Key sequence")
                       (repeat :tag "Translations" string))))

(defcustom pisigma-input-user-translations nil
  "Like `pisigma-input-translations', but more suitable for user
customizations since by default it is empty.

These translation pairs are included first, before those in
`pisigma-input-translations' and the ones inherited from other input
methods."
  :group 'pisigma-input
  :set 'pisigma-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Key sequence")
                       (repeat :tag "Translations" string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inspecting and modifying translation maps

(defun pisigma-input-get-translations (qp)
  "Return a list containing all translations from the Quail
package QP (except for those corresponding to ASCII).
Each pair in the list has the form (KEY-SEQUENCE . TRANSLATION)."
  (with-temp-buffer
    (activate-input-method qp) ; To make sure that the package is loaded.
    (unless (quail-package qp)
      (error "%s is not a Quail package." qp))
    (let ((decode-map (list 'decode-map)))
      (quail-build-decode-map (list (quail-map)) "" decode-map 0)
      (cdr decode-map))))

(defun pisigma-input-show-translations (qp)
  "Display all translations used by the Quail package QP (a string).
\(Except for those corresponding to ASCII)."
  (interactive (list (read-input-method-name
                      "Quail input method (default %s): " "PiSigma")))
  (let ((buf (concat "*" qp " input method translations*")))
    (with-output-to-temp-buffer buf
      (with-current-buffer buf
        (quail-insert-decode-map
         (cons 'decode-map (pisigma-input-get-translations qp)))))))

(defun pisigma-input-add-translations (trans)
  "Add the given translations TRANS to the PiSigma input method.
TRANS is a list of pairs (KEY-SEQUENCE . TRANSLATION). The
translations are appended to the current translations."
  (with-temp-buffer
    (dolist (tr (pisigma-input-concat-map (eval pisigma-input-tweak-all) trans))
      (quail-defrule (car tr) (cdr tr) "PiSigma" t))))

(defun pisigma-input-inherit-package (qp &optional fun)
  "Let the PiSigma input method inherit the translations from the
Quail package QP (except for those corresponding to ASCII).

The optional function FUN can be used to modify the translations.
It is given a pair (KEY-SEQUENCE . TRANSLATION) and should return
a list of such pairs."
  (let ((trans (pisigma-input-get-translations qp)))
    (pisigma-input-add-translations
     (if fun (pisigma-input-concat-map fun trans)
       trans))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up the input method

(defun pisigma-input-setup ()
  "Set up the PiSigma input method based on the customisable
variables and underlying input methods."

  ;; Create (or reset) the input method.
  (with-temp-buffer
    (quail-define-package "PiSigma" "UTF-8" "∏Σ" t ; guidance
     "PiSigma input method.
The purpose of this input method is to edit PiSigma programs, but
since it is highly customisable it can be made useful for other
tasks as well."
     nil nil nil nil nil nil t ; maximum-shortest
     ))

  (pisigma-input-add-translations
   (mapcar (lambda (tr) (cons (car tr) (vconcat (cdr tr))))
           (append pisigma-input-user-translations
                   pisigma-input-translations)))
  (dolist (def pisigma-input-inherit)
    (pisigma-input-inherit-package (car def)
                                (eval (cdr def)))))

(defun pisigma-input-incorporate-changed-setting (sym val)
  "Update the PiSigma input method based on the customisable
variables and underlying input methods.  Suitable for use in
the :set field of `defcustom'."
  (set-default sym val)
  (pisigma-input-setup))

;; Set up the input method.

(pisigma-input-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative details

(provide 'pisigma-input)
;;; pisigma-input.el ends here
