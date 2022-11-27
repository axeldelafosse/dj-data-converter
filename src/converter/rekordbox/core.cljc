#?(:clj (set! *warn-on-reflection* true))
(ns converter.rekordbox.core
  (:require
   [camel-snake-kebab.core :as csk]
   #?(:clj [clojure.spec.alpha :as s] :cljs [cljs.spec.alpha :as s])
   [clojure.data.zip.xml :as zx]
   [clojure.zip :as zip]
   [converter.config :as config]
   [converter.universal.core :as u]
   [converter.universal.marker :as um]
   [converter.rekordbox.position-mark :as rp]
   [converter.rekordbox.tempo :as rt]
   [converter.spec :as spec]
   [converter.time :as time]
   [converter.url :as url]
   [converter.xml :as xml]
   [spec-tools.data-spec :as std]
   [utils.map :as map]
   #?(:clj [taoensso.tufte :as tufte :refer (defnp p profile)]
      :cljs [taoensso.tufte :as tufte :refer-macros (defnp p profile)])))

(def xml-transformer
  (spec/xml-transformer))

(def string-transformer
  (spec/string-transformer))

(def track-spec
  (std/spec
   {:name ::track
    :spec {:tag (s/spec #{:TRACK})
           :attrs {:Location ::url/url
                   :TotalTime string?
                   (std/opt :TrackID) pos-int?
                   (std/opt :Name) string?
                   (std/opt :Artist) string?
                   (std/opt :Album) string?
                   (std/opt :Genre) string?
                   (std/opt :Label) string?
                   (std/opt :Tonality) string?
                   (std/opt :Colour) string?
                   (std/opt :Grouping) string?
                   (std/opt :TrackNumber) string?
                   (std/opt :AverageBpm) (s/double-in :min 0 :NaN? false :infinite? false)
                   (std/opt :DateAdded) (std/or {:date ::time/date
                                                 :blank (s/spec #{""})})
                   (std/opt :Comments) string?}
           :content (s/cat
                     :tempos (s/* (std/spec {:name ::tempo
                                             :spec rt/tempo-spec}))
                     :position-marks (s/* (std/spec {:name ::position-mark
                                                     :spec rp/position-mark-spec})))}}))

(defn equiv-position-marks?
  [{:keys [::u/markers]} track-z]
  (let [indexed-markers (um/indexed-markers markers)
        non-indexed-markers (um/non-indexed-markers markers)
        position-marks-z (zx/xml-> track-z :POSITION_MARK)
        position-marks-hot-cue-z (remove (comp rp/memory-cue? zip/node) position-marks-z)
        position-marks-memory-cue-z (filter (comp rp/memory-cue? zip/node) position-marks-z)
        position-marks-tagged-z (filter (comp rp/position-mark-tagged? zip/node) position-marks-z)]
    (and
     (= (count indexed-markers) (count position-marks-hot-cue-z))
     (= (count non-indexed-markers) (- (count position-marks-memory-cue-z) (count position-marks-tagged-z)))
     (every? identity
             (map
              #(= (::um/num %1) (zx/attr %2 :Num))
              indexed-markers
              position-marks-hot-cue-z)))))

(s/fdef item->track
  :args (s/cat :item (spec/such-that-spec u/item-spec u/item-contains-total-time? 100))
  :ret track-spec
  :fn (fn equiv-track? [{{conformed-item :item} :args conformed-track :ret}]
        (let [item (s/unform u/item-spec conformed-item)
              track-z (zip/xml-zip (s/unform track-spec conformed-track))]
          (and
           (= (::u/title item) (zx/attr track-z :Name))
           (= (::u/artist item) (zx/attr track-z :Artist))
           (= (::u/total-time item) (zx/attr track-z :TotalTime))
           (= (::u/comments item) (zx/attr track-z :Comments))
           (= (::u/genre item) (zx/attr track-z :Genre))
           (= (::u/label item) (zx/attr track-z :Label))
           (= (::u/musical-key item) (zx/attr track-z :Tonality))
           (= (::u/color item) (zx/attr track-z :Colour))
           (equiv-position-marks? item track-z))))) ; TODO equiv-track-tempos

; TODO move to position-mark ns?
(defn marker->position-marks
  [non-indexed-markers marker]
  (cond-> []
    true (conj (rp/marker->position-mark marker (um/non-indexed-marker? marker)))
    (and (not (um/non-indexed-marker? marker))
         (not (um/matching-marker? non-indexed-markers marker))) (conj (rp/marker->position-mark-tagged marker true))))

(defn item->track
  [{:keys [::u/title ::u/bpm ::u/key ::u/musical-key ::u/color ::u/markers ::u/tempos] :as item}]
  (p ::item->track
     {:tag :TRACK
      :attrs (cond-> item
               true (-> (dissoc ::u/title ::u/bpm ::u/key ::u/musical-key ::u/color ::u/markers ::u/tempos) (map/transform-keys csk/->PascalCaseKeyword))
               title (assoc :Name title)
               bpm (assoc :AverageBpm bpm)
               ; (contains? equiv-text-key-to-classic key) (assoc :Tonality (get equiv-text-key-to-classic key))
               (contains? equiv-musical-key-to-classic musical-key) (assoc :Tonality (get equiv-musical-key-to-classic musical-key))
               (contains? equiv-traktor-color-to-rekordbox-colour color) (assoc :Colour (get equiv-traktor-color-to-rekordbox-colour color))
               (contains? equiv-traktor-color-to-rekordbox-grouping color) (assoc :Grouping (get equiv-traktor-color-to-rekordbox-grouping color)))
      :content (cond-> []
                 tempos (concat (map rt/item-tempo->tempo tempos))
                 markers (concat (reduce #(concat %1 (marker->position-marks (um/non-indexed-markers markers) %2)) [] markers)))}))

(defn equiv-markers?
  [track-z {:keys [::u/markers]}]
  (let [position-marks-z (remove (comp rp/position-mark-tagged? zip/node) (zx/xml-> track-z :POSITION_MARK))]
    (= (count position-marks-z) (count markers))))

(defn equiv-tempos?
  [track-z {:keys [::u/tempos]}]
  (let [tempos-z (zx/xml-> track-z :TEMPO)]
    (= (count tempos-z) (count tempos))))

(s/fdef track->item
  :args (s/cat :track (spec/xml-zip-spec track-spec))
  :ret u/item-spec
  :fn (fn equiv-item? [{{conformed-track :track} :args conformed-item :ret}]
        (let [track-z (zip/xml-zip (s/unform track-spec conformed-track))
              item (s/unform u/item-spec conformed-item)]
          (and
           (= (zx/attr track-z :Name) (::u/title item))
           (= (zx/attr track-z :Artist) (::u/artist item))
           (= (zx/attr track-z :Genre) (::u/genre item))
           (= (zx/attr track-z :TotalTime) (::u/total-time item))
           (= (zx/attr track-z :Comments) (::u/comments item))
           (= (zx/attr track-z :Label) (::u/label item))
           ; (= (zx/attr track-z :Tonality) (::u/key item))
           ; (= (zx/attr track-z :Tonality) (::u/musical-key item)) ; TODO equiv-classic-key-to-musical-key
           ; (= (zx/attr track-z :Colour) (::u/color item)) ; TODO equiv-rekordbox-colour-to-traktor-color
           (equiv-tempos? track-z item)
           (equiv-markers? track-z item)))))

(defn track->item
  [track-z]
  (p ::track->item
     (let [tempos-z (zx/xml-> track-z :TEMPO)
           position-marks-z (remove (comp rp/position-mark-tagged? zip/node) (zx/xml-> track-z :POSITION_MARK))
           Name (zx/attr track-z :Name)
           AverageBpm (zx/attr track-z :AverageBpm)
           Label (zx/attr track-z :Label)
           Tonality (zx/attr track-z :Tonality)
           Colour (zx/attr track-z :Colour)
           DateAdded (zx/attr track-z :DateAdded)]
       (cond-> track-z
         true (-> zip/node
                  :attrs
                  (dissoc :Name :AverageBpm :Tonality :Colour :DateAdded)
                  (map/transform-keys (comp #(keyword (namespace ::u/unused) %) csk/->kebab-case name)))
         Name (assoc ::u/title Name)
         AverageBpm (assoc ::u/bpm AverageBpm)
         ; Tonality (assoc ::u/key Tonality)
         ; Tonality (assoc ::u/musical-key Tonality) ; TODO equiv-key (to Traktor musical key)
         ; Colour (assoc ::u/color Colour) ; TODO equiv-rekordbox-colour-to-traktor-color
         (and DateAdded (not (string? DateAdded))) (assoc ::u/date-added DateAdded)
         (not-empty tempos-z) (assoc ::u/tempos (map rt/tempo->item-tempo tempos-z))
         (not-empty position-marks-z) (assoc ::u/markers (map rp/position-mark->marker position-marks-z))))))

(defn library->dj-playlists
  [{:keys [progress]} _ {:keys [::u/collection]}]
  {:tag :DJ_PLAYLISTS
   :attrs {:Version "1.0.0"}
   :content [{:tag :COLLECTION
              :content (map (progress item->track)
                            ; Rekordbox xml tracks must have a total time
                            ; TODO summarize what was done (i.e. items without total time filtered out) in a report
                            (filter u/item-contains-total-time? collection))}]})

(defn dj-playlists->library
  [_ dj-playlists]
  (if (xml/xml? dj-playlists)
    (let [dj-playlists-z (zip/xml-zip dj-playlists)
          collection-z (zx/xml1-> dj-playlists-z :COLLECTION)]
      {::u/collection (map track->item (zx/xml-> collection-z :TRACK))})
    dj-playlists))

(def collection-spec
  (std/spec
   {:name ::collection
    :spec {:tag (s/spec #{:COLLECTION})
           :content (s/cat :tracks (s/* track-spec))}}))

(def dj-playlists
  {:tag (s/spec #{:DJ_PLAYLISTS})
   :attrs {:Version string?}
   :content (s/cat
             :product (s/? (std/spec {:name ::product
                                      :spec {:tag (s/spec #{:PRODUCT})}}))
             :collection collection-spec
             :playlists (s/? (std/spec {:name ::playlists
                                        :spec {:tag (s/spec #{:PLAYLISTS})}})))})

(defn dj-playlists-spec
  [config]
  (->
   (std/spec
    {:name ::dj-playlists
     :spec dj-playlists})
   (assoc :encode/xml (partial library->dj-playlists config))))

(s/fdef dj-playlists->library
  :args (s/cat :dj-playlists-spec any? :dj-playlists (dj-playlists-spec {}))
  :ret u/library-spec
  :fn (fn equiv-collection-counts?
        [{{conformed-dj-playlists :dj-playlists} :args conformed-library :ret}]
        (let [dj-playlists (s/unform (dj-playlists-spec {}) conformed-dj-playlists)
              dj-playlists-z (zip/xml-zip dj-playlists)
              library (s/unform u/library-spec conformed-library)]
          (=
           (count (zx/xml-> dj-playlists-z :COLLECTION :TRACK))
           (count (::u/collection library))))))

(s/fdef library->dj-playlists
  :args (s/cat :config ::config/config :library-spec any? :library u/library-spec)
  :ret (dj-playlists-spec {})
  :fn (fn equiv-collection-counts?
        [{{conformed-library :library} :args conformed-dj-playlists :ret}]
        (let [library (s/unform u/library-spec conformed-library)
              dj-playlists (s/unform (dj-playlists-spec {}) conformed-dj-playlists)
              dj-playlists-z (zip/xml-zip dj-playlists)]
          (= (count (->> library ::u/collection (filter u/item-contains-total-time?)))
             (count (zx/xml-> dj-playlists-z :COLLECTION :TRACK))))))

(def library-spec
  (-> u/library-spec
      (assoc :decode/xml dj-playlists->library)))

(def equiv-text-key-to-classic
  {"1m"  "Am"
   "2m"  "Em"
   "3m"  "Bm"
   "4m"  "F#m"
   "5m"  "Dbm"
   "6m"  "Abm"
   "7m"  "Ebm"
   "8m"  "Bbm"
   "9m"  "Fm"
   "10m" "Cm"
   "11m" "Gm"
   "12m" "Dm"
   "1d"  "C"
   "2d"  "G"
   "3d"  "D"
   "4d"  "A"
   "5d"  "E"
   "6d"  "B"
   "7d"  "F#"
   "8d"  "Db"
   "9d"  "Ab"
   "10d" "Eb"
   "11d" "Bb"
   "12d" "F"
   "8A"  "Am"
   "9A"  "Em"
   "10A" "Bm"
   "11A" "F#m"
   "12A" "Dbm"
   "1A"  "Abm"
   "2A"  "Ebm"
   "3A"  "Bbm"
   "4A"  "Fm"
   "5A"  "Cm"
   "6A"  "Gm"
   "7A"  "Dm"
   "8B"  "C"
   "9B"  "G"
   "10B" "D"
   "11B" "A"
   "12B" "E"
   "1B"  "B"
   "2B"  "F#"
   "3B"  "Db"
   "4B"  "Ab"
   "5B"  "Eb"
   "6B"  "Bb"
   "7B"  "F"
   "Am"  "Am"
   "Em"  "Em"
   "Bm"  "Bm"
   "F#m" "F#m"
   "Dbm" "Dbm"
   "Abm" "Abm"
   "Ebm" "Ebm"
   "Bbm" "Bbm"
   "Fm"  "Fm"
   "Cm"  "Cm"
   "Gm"  "Gm"
   "Dm"  "Dm"
   "C"   "C"
   "G"   "G"
   "D"   "D"
   "A"   "A"
   "E"   "E"
   "B"   "B"
   "F#"  "F#"
   "Db"  "Db"
   "Ab"  "Ab"
   "Eb"  "Eb"
   "Bb"  "Bb"
   "F"   "F"})

(def equiv-musical-key-to-classic
  {"21"  "Am"
   "16"  "Em"
   "23"  "Bm"
   "18"  "F#m"
   "13"  "Dbm"
   "20"  "Abm"
   "15"  "Ebm"
   "22"  "Bbm"
   "17"  "Fm"
   "12" "Cm"
   "19" "Gm"
   "14" "Dm"
   "0"  "C"
   "7"  "G"
   "2"  "D"
   "9"  "A"
   "4"  "E"
   "11"  "B"
   "6"  "F#"
   "1"  "Db"
   "8"  "Ab"
   "3" "Eb"
   "10" "Bb"
   "5" "F"})

(def equiv-classic-key-to-open
  {"Am" "1m"
   "Em" "2m"
   "Bm" "3m"
   "F#m" "4m"
   "Dbm" "5m"
   "Abm" "6m"
   "Ebm" "7m"
   "Bbm" "8m"
   "Fm" "9m"
   "Cm" "10m"
   "Gm" "11m"
   "Dm" "12m"
   "C" "1d"
   "G" "2d"
   "D" "3d"
   "A" "4d"
   "E" "5d"
   "B" "6d"
   "F#" "7d"
   "Db" "8d"
   "Ab" "9d"
   "Eb" "10d"
   "Bb" "11d"
   "F" "12d"})

; Rekordbox: Pink, Red, Orange, Yellow, Green, Aqua, Blue, Purple
; Traktor: Red, Orange, Yellow, Green, Blue (+ Aqua), Violet, Magenta

(def equiv-traktor-color-to-rekordbox-grouping
  {"1" "Red"
   "2" "Orange"
   "3" "Yellow"
   "4" "Green"
   "5" "Blue"
   "6" "Purple"
   "7" "Pink"})

(def equiv-rekordbox-grouping-to-traktor-color
  {"Red" "1"
   "Orange" "2"
   "Yellow" "3"
   "Green" "4"
   "Aqua" "5" ; Traktor doesn't have Aqua
   "Blue" "5"
   "Purple" "6"
   "Pink" "7"})

(def equiv-traktor-color-to-rekordbox-colour
  {"1" "0xFF0000"
   "2" "0xFFA500"
   "3" "0xFFFF00"
   "4" "0x00FF00"
   "5" "0x0000FF"
   "6" "0x660099"
   "7" "0xFF007F"})

(def equiv-rekordbox-colour-to-traktor-color
  {"0xFF0000" "1"
   "0xFFA500" "2"
   "0xFFFF00" "3"
   "0x00FF00" "4"
   "0x25FDE9" "5" ; Aqua -> Blue
   "0x0000FF" "5"
   "0x660099" "6"
   "0xFF007F" "7"})