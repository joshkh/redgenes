(ns bluegenes.lists-test
  (:require [cljs.test :refer-macros [deftest is are testing]]
            [bluegenes.pages.lists.utils :as utils]))

(deftest path-title
  (is (= "nested folder" (utils/path-title "bgpath:my folder.nested folder")))
  (is (= "my folder" (utils/path-title "bgpath:my folder"))))

(deftest top-level-predicates
  (is (true? (utils/top-level-folder? {:path "bgpath:my folder"})))
  (is (false? (utils/top-level-folder? {:id "foo"})))
  (is (false? (utils/top-level-list? {:path "bgpath:my folder" :tags nil}))))

(deftest list->path
  (is (= "bgpath:foo" (utils/list->path {:tags ["foo" "bgpath:foo"]}))))

(deftest path-relation
  (is (false? (utils/child-of? "bgpath:foo.bar" "bgpath:foo.bar")))
  (is (true? (utils/child-of? "bgpath:foo.bar" "bgpath:foo.bar.baz")))
  (is (false? (utils/child-of? "bgpath:foo.bar" "bgpath:foo.bar.baz.boz")))
  (is (true? (utils/descendant-of? "bgpath:foo.bar" "bgpath:foo.bar.baz.boz")))
  (is (= 2 (utils/nesting "bgpath:foo.bar.baz"))))

(deftest ensure-subpaths
  (is (= '("bgpath:foo" "bgpath:foo.bar" "bgpath:bar" "bgpath:bar.bar")
         (utils/ensure-subpaths '("bgpath:foo.bar" "bgpath:bar.bar")))))

(def normalized
  [{:description "Simple gene list created for testing",
    :tags [],
    :authorized false,
    :name
    "Gene List (Fri May 29 2020 18:05:48 GMT+0100 (British Summer Time))",
    :type "Gene",
    :size 17771,
    :title
    "Gene List (Fri May 29 2020 18:05:48 GMT+0100 (British Summer Time))",
    :status "CURRENT",
    :id 10000001,
    :timestamp 1590771950669,
    :dateCreated "2020-05-29T18:05:50+0100"}
   {:description "",
    :tags ["bgpath:my folder.My First List"],
    :authorized true,
    :name "My First List",
    :type "Gene",
    :size 25,
    :title "My First List",
    :status "CURRENT",
    :id 8000003,
    :timestamp 1584024256849,
    :dateCreated "2020-03-12T14:44:16+0000"}
   {:tags ["bgpath:my folder.My First List"],
    :authorized true,
    :name "My First List_1",
    :type "Gene",
    :size 25,
    :title "My First List_1",
    :status "CURRENT",
    :id 11000014,
    :timestamp 1591881004534,
    :dateCreated "2020-06-11T14:10:04+0100"}
   {:description "",
    :tags ["bgpath:my folder.My First List"],
    :authorized true,
    :name "My First List_2",
    :type "Gene",
    :size 25,
    :title "My First List_2",
    :status "CURRENT",
    :id 14000064,
    :timestamp 1595950686476,
    :dateCreated "2020-07-28T16:38:06+0100"}
   {:tags ["im:public"],
    :authorized true,
    :name "Public ABC Genes (testing)",
    :type "Gene",
    :size 143,
    :title "Public ABC Genes (testing)",
    :status "CURRENT",
    :id 14000097,
    :timestamp 1596530623177,
    :dateCreated "2020-08-04T09:43:43+0100"}])

(def denormalized
  {10000001 {:description "Simple gene list created for testing",
             :tags [],
             :authorized false,
             :name
             "Gene List (Fri May 29 2020 18:05:48 GMT+0100 (British Summer Time))",
             :type "Gene",
             :size 17771,
             :title
             "Gene List (Fri May 29 2020 18:05:48 GMT+0100 (British Summer Time))",
             :status "CURRENT",
             :id 10000001,
             :timestamp 1590771950669,
             :dateCreated "2020-05-29T18:05:50+0100"},
   8000003 {:description "",
            :tags ["bgpath:my folder.My First List"],
            :authorized true,
            :name "My First List",
            :type "Gene",
            :size 25,
            :title "My First List",
            :status "CURRENT",
            :id 8000003,
            :timestamp 1584024256849,
            :dateCreated "2020-03-12T14:44:16+0000"},
   11000014 {:tags ["bgpath:my folder.My First List"],
             :authorized true,
             :name "My First List_1",
             :type "Gene",
             :size 25,
             :title "My First List_1",
             :status "CURRENT",
             :id 11000014,
             :timestamp 1591881004534,
             :dateCreated "2020-06-11T14:10:04+0100"},
   14000064 {:description "",
             :tags ["bgpath:my folder.My First List"],
             :authorized true,
             :name "My First List_2",
             :type "Gene",
             :size 25,
             :title "My First List_2",
             :status "CURRENT",
             :id 14000064,
             :timestamp 1595950686476,
             :dateCreated "2020-07-28T16:38:06+0100"},
   14000097 {:tags ["im:public"],
             :authorized true,
             :name "Public ABC Genes (testing)",
             :type "Gene",
             :size 143,
             :title "Public ABC Genes (testing)",
             :status "CURRENT",
             :id 14000097,
             :timestamp 1596530623177,
             :dateCreated "2020-08-04T09:43:43+0100"},
   "bgpath:my folder" {:id "bgpath:my folder",
                       :path "bgpath:my folder",
                       :title "my folder",
                       :size 1,
                       :authorized true,
                       :dateCreated "2020-07-28T16:38:06+0100",
                       :timestamp 1595950686476,
                       :children '("bgpath:my folder.My First List")},
   "bgpath:my folder.My First List" {:id "bgpath:my folder.My First List",
                                     :path "bgpath:my folder.My First List",
                                     :title "My First List",
                                     :size 3,
                                     :authorized true,
                                     :dateCreated "2020-07-28T16:38:06+0100",
                                     :timestamp 1595950686476,
                                     :children '(8000003 11000014 14000064)}})

(deftest denormalize-lists
  (is (= denormalized (utils/denormalize-lists normalized))))

(deftest normalize-lists
  (let [normalized-1 [{:description "Simple gene list created for testing",
                       :tags [],
                       :authorized false,
                       :name
                       "Gene List (Fri May 29 2020 18:05:48 GMT+0100 (British Summer Time))",
                       :type "Gene",
                       :size 17771,
                       :title
                       "Gene List (Fri May 29 2020 18:05:48 GMT+0100 (British Summer Time))",
                       :status "CURRENT",
                       :id 10000001,
                       :timestamp 1590771950669,
                       :dateCreated "2020-05-29T18:05:50+0100"}
                      {:tags ["im:public"],
                       :authorized true,
                       :name "Public ABC Genes (testing)",
                       :type "Gene",
                       :size 143,
                       :title "Public ABC Genes (testing)",
                       :status "CURRENT",
                       :id 14000097,
                       :timestamp 1596530623177,
                       :dateCreated "2020-08-04T09:43:43+0100"}
                      {:id "bgpath:my folder",
                       :path "bgpath:my folder",
                       :title "my folder",
                       :size 1,
                       :authorized true,
                       :dateCreated "2020-07-28T16:38:06+0100",
                       :timestamp 1595950686476,
                       :children '("bgpath:my folder.My First List")}]
        normalized-2 [{:description "Simple gene list created for testing",
                       :tags [],
                       :authorized false,
                       :name
                       "Gene List (Fri May 29 2020 18:05:48 GMT+0100 (British Summer Time))",
                       :type "Gene",
                       :size 17771,
                       :title
                       "Gene List (Fri May 29 2020 18:05:48 GMT+0100 (British Summer Time))",
                       :status "CURRENT",
                       :id 10000001,
                       :timestamp 1590771950669,
                       :dateCreated "2020-05-29T18:05:50+0100"}
                      {:tags ["im:public"],
                       :authorized true,
                       :name "Public ABC Genes (testing)",
                       :type "Gene",
                       :size 143,
                       :title "Public ABC Genes (testing)",
                       :status "CURRENT",
                       :id 14000097,
                       :timestamp 1596530623177,
                       :dateCreated "2020-08-04T09:43:43+0100"}
                      {:id "bgpath:my folder",
                       :path "bgpath:my folder",
                       :title "my folder",
                       :size 1,
                       :authorized true,
                       :dateCreated "2020-07-28T16:38:06+0100",
                       :timestamp 1595950686476,
                       :children '("bgpath:my folder.My First List")}]
        normalized-3 [{:description "Simple gene list created for testing",
                       :tags [],
                       :authorized false,
                       :name
                       "Gene List (Fri May 29 2020 18:05:48 GMT+0100 (British Summer Time))",
                       :type "Gene",
                       :size 17771,
                       :title
                       "Gene List (Fri May 29 2020 18:05:48 GMT+0100 (British Summer Time))",
                       :status "CURRENT",
                       :id 10000001,
                       :timestamp 1590771950669,
                       :dateCreated "2020-05-29T18:05:50+0100"}
                      {:tags ["im:public"],
                       :authorized true,
                       :name "Public ABC Genes (testing)",
                       :type "Gene",
                       :size 143,
                       :title "Public ABC Genes (testing)",
                       :status "CURRENT",
                       :id 14000097,
                       :timestamp 1596530623177,
                       :dateCreated "2020-08-04T09:43:43+0100"}
                      {:id "bgpath:my folder",
                       :path "bgpath:my folder",
                       :title "my folder",
                       :size 1,
                       :authorized true,
                       :dateCreated "2020-07-28T16:38:06+0100",
                       :timestamp 1595950686476,
                       :children '("bgpath:my folder.My First List")}
                      {:path "bgpath:my folder.My First List",
                       :children '(8000003 11000014 14000064),
                       :authorized true,
                       :size 3,
                       :title "My First List",
                       :id "bgpath:my folder.My First List",
                       :timestamp 1595950686476,
                       :dateCreated "2020-07-28T16:38:06+0100",
                       :is-last true}]]
    (is (= normalized-1 (utils/normalize-lists identity identity {:by-id denormalized :expanded-paths #{}})))
    (is (= normalized-2 (utils/normalize-lists identity identity {:by-id denormalized :expanded-paths #{"bgpath:my folder.nested folder"}})))
    (is (= normalized-3 (utils/normalize-lists identity identity {:by-id denormalized :expanded-paths #{"bgpath:my folder"}})))))

(deftest filtered-list-ids-set
  (is (= #{14000097 8000003 14000064 11000014 10000001}
         (utils/filtered-list-ids-set denormalized {}))))

(deftest copy-list-name
  (is (= "UseCase1_transcripts_oxidativeStress_1_1"
         (utils/copy-list-name [{:name "UseCase1_transcripts_oxidativeStress_1"}]
                               "UseCase1_transcripts_oxidativeStress_1")))
  (is (= "foo_1"
         (utils/copy-list-name [{:name "foo"}] "foo")))
  (is (= "foo_1"
         (utils/copy-list-name [{:name "foo_"}] "foo")))
  (is (= "foo_2"
         (utils/copy-list-name [{:name "foo"} {:name "foo_1"}] "foo")))
  (is (= "foo_10"
         (utils/copy-list-name [{:name "foo"} {:name "foo_9"}] "foo")))
  (is (= "foo_11"
         (utils/copy-list-name [{:name "foo"} {:name "foo_9"} {:name "foo_10"}] "foo")))
  (is (= "foo_11"
         (utils/copy-list-name [{:name "foo"} {:name "foo_10"}] "foo"))))
