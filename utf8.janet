(def- utf8-char-peg
  ~{:more (range "\x80\xBF")
    # 1 byte variant (0xxxxxxx)
    :1byte (/ '(range "\x00\x7F") ,first)
    # 2 byte variant (110xxxxx 10xxxxxx)
    :2bytes (/ '(* (range "\xC0\xDF") :more)
                ,(fn [[x y]]
                   (bor (blshift (band x 0x1F) 6)
                        (band y 0x3F))))
    # 3 byte variant (1110xxxx 10xxxxxx 10xxxxxx)
    :3bytes (/ '(* (range "\xE0\xEF") :more :more)
                ,(fn [[x y z]]
                   (bor (blshift (band x 0x0F) 12)
                        (blshift (band y 0x3F) 6)
                        (band z 0x3F))))
    # 4 byte variant (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
    :4bytes (/ '(* (range "\xF0\xF7") :more :more :more)
                ,(fn [[x y z w]]
                   (bor (blshift (band x 0x07) 18)
                        (blshift (band y 0x3F) 12)
                        (blshift (band z 0x3F) 6)
                        (band w 0x3F))))
    :char (+ :1byte :2bytes :3bytes :4bytes
             (error "Not UTF-8"))
    :main :char})

(def- utf8-char
  (peg/compile utf8-char-peg))

(def- utf8-string
  (peg/compile (table/to-struct
                (merge utf8-char-peg '{:main (any :char)}))))

(defn decode-char
  "Convert the first UTF-8 character into a unicode point"
  [str]
  (get (peg/match utf8-char str) 0))

(defn decode
  "Convert a UTF-8 string into unicode points."
  [str]
  (peg/match utf8-string str))

(defn encode
  "Convert unicode points into a UTF-8 string."
  [u]
  (let [buf @""]
    (each c u
      (cond
        # 1 byte variant (0xxxxxxx)
        (< c 0x80)
        (buffer/format buf "%c" c)
        # 2 byte variant (110xxxxx 10xxxxxx)
        (< c 0x800)
        (buffer/format buf "%c%c"
                       (bor 0xC0 (brshift c 6))
                       (bor 0x80 (band 0x3F c)))
        # 3 byte variant (1110xxxx 10xxxxxx 10xxxxxx)
        (< c 0x10000)
        (buffer/format buf "%c%c%c"
                       (bor 0xE0 (brshift c 12))
                       (bor 0x80 (band 0x3F (brshift c 6)))
                       (bor 0x80 (band 0x3F c)))
        # 4 byte variant (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
        (< c 0x110000)
        (buffer/format buf "%c%c%c%c"
                       (bor 0xF0 (brshift c 18))
                       (bor 0x80 (band 0x3F (brshift c 12)))
                       (bor 0x80 (band 0x3F (brshift c 6)))
                       (bor 0x80 (band 0x3F c)))
        (error (string "Invalid unicode point: " c))))
    (string buf)))
