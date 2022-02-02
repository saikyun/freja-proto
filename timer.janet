(import freja/state)
(import freja/events :as e)
(use freja/defonce)
(use freja/flow)

(unless (state/editor-state :inited-audio)
  (init-audio-device)
  (put state/editor-state :inited-audio true))

(defonce sound (load-sound "assets/lugnanerej.mp3"))

(when-let [fib (get-in (dyn 'state) [:ref 0 :fib])]
  (ev/cancel fib "stop timer"))

(def state @{:freja/label "Timer"
             :time 0})

(defn refresh-timer
  [minutes]
  (fn []
    (loop [i :range [0 (inc (* 60 minutes))]]
      (put state :time i)
      (e/put! state/editor-state :force-refresh true)
      (ev/sleep 1))
    (play-sound sound)
    (put state :alarm true)
    (e/put! state/editor-state :force-refresh true)))

(defn start-timer
  [minutes]
  (when-let [fib (state :fib)]
    (ev/cancel fib "stop timer"))
  (put state :fib (ev/call (refresh-timer minutes))))

(defn timer
  [state]
  (def {:time time
        :alarm alarm} state)
  [:block {}
   [:padding {:all 2}
    [:clickable {:on-click (fn [_] (start-timer 25))}
     [:padding {:right 6}
      [:text {:size 12
              :color [0.5 0.5 0.5]
              :text "Go"}]]]
    [:text {:color (if alarm :green [0.6 0.6 0.6])
            :size (if alarm
                    60
                    12)
            :text (string/format "%02d:%02d" (math/floor (/ time 60))
                                 (mod time 60))}]]])

(comment
  (start-timer 25)

  (e/put! state/editor-state :other [timer state])
  #
)
