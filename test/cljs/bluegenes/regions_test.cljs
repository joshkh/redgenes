(ns bluegenes.regions-test
  (:require [cljs.test :refer-macros [deftest is are testing]]
            [bluegenes.pages.regions.events :refer [parse-region]]))

(deftest parsing-genome-regions

  (testing "Accepted formats in help text"
    (are [in out] (= (parse-region nil in) out)
      "2L:11334..12296"
      {:chromosome "2L" :from 11334 :to 12296}
      "2R:5866746-5868284"
      {:chromosome "2R" :from 5866746 :to 5868284}
      "chrII:14646344-14667746"
      {:chromosome "chrII" :from 14646344 :to 14667746}
      "3R:2578486:2580016:-1"
      {:chromosome "3R" :from 2578486 :to 2580016 :strand "-1"}
      "2L:14615455:14619002:1"
      {:chromosome "2L" :from 14615455 :to 14619002 :strand "1"}
      "2L\t11334\t12296"
      {:chromosome "2L" :from 11334 :to 12296}))

  (testing "Weird chromosome and strand identifiers"
    (are [in out] (= (parse-region nil in) out)
      "mtDNA:11334..12296"
      {:chromosome "mtDNA" :from 11334 :to 12296}
      "chromosome_1:11334..12296"
      {:chromosome "chromosome_1" :from 11334 :to 12296}
      "you wouldn't put spaces in your chromosome:11334..12296"
      {:chromosome "you wouldn't put spaces in your chromosome" :from 11334 :to 12296}
      "ch:11334..12296:reverse"
      {:chromosome "ch" :from 11334 :to 12296 :strand "reverse"}
      "ch:11334..12296:there are only two strands right?"
      {:chromosome "ch" :from 11334 :to 12296 :strand "there are only two strands right?"}))

  (is (= (parse-region nil "2L:12296..11334")
         {:chromosome "2L" :from 11334 :to 12296})
      "Should handle descending coordinates")

  (testing "Leading and trailing whitespace"
    (is (= (parse-region nil "  2L:12296..11334  ")
           {:chromosome "2L" :from 11334 :to 12296})
        "Should be removed")
    (is (= (parse-region nil "  2L:12296:11334:-1  ")
           {:chromosome "2L" :from 11334 :to 12296 :strand "-1"})
        "Should be removed also after strand")

    (testing "Nil when instead of chromosome and/or strand"
      (are [in] (nil? (parse-region nil in))
        "  :12296:11334:  "
        "  :12296..11334  "
        "  12296..11334  ")))

  (testing "Interbase coordinate system"
    (is (= (parse-region {:coordinates :interbase} "2L:11334..12296")
           {:chromosome "2L" :from 11335 :to 12296})
        "Should translate interbase coordinates")
    (is (= (parse-region {:coordinates :interbase} "2L:12296..11334")
           {:chromosome "2L" :from 11335 :to 12296})
        "Should translate interbase coordinates also for descending coordinates"))

  (testing "Strand-specific search"
    (is (= (parse-region {:strand-specific true} "2L:11334..12296")
           {:chromosome "2L" :from 11334 :to 12296 :strand "1"})
        "Ascending coordinates should use plus strand")
    (is (= (parse-region {:strand-specific true} "2L:12296..11334")
           {:chromosome "2L" :from 11334 :to 12296 :strand "-1"})
        "Descending coordinates should use minus strand")
    (is (= (parse-region {:coordinates :interbase :strand-specific true} "2L:12296..11334")
           {:chromosome "2L" :from 11335 :to 12296 :strand "-1"})
        "Should still translate interbase coordinates")
    (is (= (parse-region {:strand-specific true} "2L:12296:11334:1")
           {:chromosome "2L" :from 11334 :to 12296 :strand "1"})
        "Should be overridden when explicitly specifying strand")
    (is (= (parse-region {:coordinates :interbase :strand-specific true} "2L:12296:11334:1")
           {:chromosome "2L" :from 11335 :to 12296 :strand "1"})
        "Should be overridden when explicitly specifying strand and still translate interbase coordinates"))

  (testing "Extend genome region"
    (is (= (parse-region {:extend-start "0" :extend-end "0"} "2L:11334..12296")
           {:chromosome "2L" :from 11334 :to 12296})
        "Should not extend region if zero")
    (is (= (parse-region {:extend-start "50" :extend-end "50"} "2L:11334..12296")
           {:chromosome "2L" :from 11284 :to 12346})
        "Should extend region by equal amounts")
    (is (= (parse-region {:extend-start "334" :extend-end "4"} "2L:11334..12296")
           {:chromosome "2L" :from 11000 :to 12300})
        "Should extend region by differing amounts")
    (is (= (parse-region {:extend-start "334.4" :extend-end "4.15"} "2L:11334..12296")
           {:chromosome "2L" :from 11000 :to 12300})
        "Should ignore decimals when extending region")
    (is (= (parse-region {:extend-start "1k" :extend-end "1M"} "2L:11334..12296")
           {:chromosome "2L" :from 10334 :to 1012296})
        "k and M base pair notation should convert to numbers")
    (is (= (parse-region {:extend-start "10.23k" :extend-end "1.01M"} "2L:11334..12296")
           {:chromosome "2L" :from 1104 :to 1022296})
        "k and M base pair notation with decimals should convert to numbers")
    (is (= (parse-region {:extend-start "10.23015k" :extend-end "1.010000002M"} "2L:11334..12296")
           {:chromosome "2L" :from 1104 :to 1022296})
        "k and M base pair notation resulting in decimals should be ignored")
    (is (= (parse-region {:extend-start "100k" :extend-end "0"} "2L:11334..12296")
           {:chromosome "2L" :from 0 :to 12296})
        "From coordinate should be zero if extended to negative")

    (is (= (parse-region {:extend-start "1k" :extend-end "4" :coordinates :interbase} "2L:11334..12296")
           {:chromosome "2L" :from 10335 :to 12300})
        "Should work together with interbase coordinates")
    (is (= (parse-region {:extend-start "100k" :extend-end "0" :coordinates :interbase} "2L:11334..12296")
           {:chromosome "2L" :from 0 :to 12296})
        "Interbase from coordinate should not be translated after extending region")

    (is (= (parse-region {:extend-start "34" :extend-end "4" :strand-specific true} "2L:11334..12296")
           {:chromosome "2L" :from 11300 :to 12300 :strand "1"})
        "Should work together with strand-specific search using ascending coordinates")
    (is (= (parse-region {:extend-start "34" :extend-end "4" :strand-specific true} "2L:12296..11334")
           {:chromosome "2L" :from 11300 :to 12300 :strand "-1"})
        "Should work together with strand-specific search using descending coordinates")
    (is (= (parse-region {:extend-start "35" :extend-end "4" :strand-specific true :coordinates :interbase} "2L:12296..11334")
           {:chromosome "2L" :from 11300 :to 12300 :strand "-1"})
        "Should work together with strand-specific search using descending coordinates in interbase")))
