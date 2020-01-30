(def- utf8-peg
  (peg/compile
   ~{:more (range "\x80\xBF")
     # 1 byte variant (0xxxxxxx)
     :1byte (/ (<- (range "\x00\x7F")) ,first)
     # 2 byte variant (110xxxxx 10xxxxxx)
     :2bytes (/ (<- (* (range "\xC0\xDF") :more))
                ,(fn [[x y]]
                   (bor (blshift (band x 0x1F) 6)
                        (band y 0x3F))))
     # 3 byte variant (1110xxxx 10xxxxxx 10xxxxxx)
     :3bytes (/ (<- (* (range "\xE0\xEF") :more :more))
                ,(fn [[x y z]]
                   (bor (blshift (band x 0x0F) 12)
                        (blshift (band y 0x3F) 6)
                        (band z 0x3F))))
     # 4 byte variant (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
     :4bytes (/ (<- (* (range "\xF0\xF7") :more :more :more))
                ,(fn [[x y z w]]
                   (bor (blshift (band x 0x07) 18)
                        (blshift (band y 0x3F) 12)
                        (blshift (band z 0x3F) 6)
                        (band w 0x3F))))
     :main (any (+ :1byte :2bytes :3bytes :4bytes
                   (error "Not UTF-8")))}))

(defn decode
  "Convert a UTF-8 string into unicode points."
  [str]
  (peg/match utf8-peg str))

(defn encode
  "Convert unicode points into a UTF-8 string."
  [u]
  (let [conv |(cond
                # 1 byte variant (0xxxxxxx)
                (< $ 0x80)
                (string/format "%c" $)
                # 2 byte variant (110xxxxx 10xxxxxx)
                (< $ 0x800)
                (string/format "%c%c"
                               (bor 0xC0 (brshift $ 6))
                               (bor 0x80 (band 0x3F $)))
                # 3 byte variant (1110xxxx 10xxxxxx 10xxxxxx)
                (< $ 0x10000)
                (string/format "%c%c%c"
                               (bor 0xE0 (brshift $ 12))
                               (bor 0x80 (band 0x3F (brshift $ 6)))
                               (bor 0x80 (band 0x3F $)))
                # 4 byte variant (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
                (< $ 0x110000)
                (string/format "%c%c%c%c"
                               (bor 0xF0 (brshift $ 18))
                               (bor 0x80 (band 0x3F (brshift $ 12)))
                               (bor 0x80 (band 0x3F (brshift $ 6)))
                               (bor 0x80 (band 0x3F $)))
                (error (string "Invalid unicode point: " $)))]
    (apply string (map conv u))))
