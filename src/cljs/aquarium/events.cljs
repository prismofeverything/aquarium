(ns aquarium.events
  (:require [domina.events :as events]
            [cljs.core.async :refer [chan <! >! put!]]
            [cljs.reader :as reader]
            [aquarium.skeleton :as skeleton]
            [aquarium.scene :as scene])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn log
  [e]
  (.log js/console e))

(def send (chan))
(def receive (chan))

(def ws-url "ws://localhost:19991/async")
(def ws (new js/WebSocket ws-url))

(defn init-listeners
  [{:keys [key-down key-up mouse-down mouse-up mouse-move resize]}]
  (.addEventListener js/document "keydown" key-down false)
  (.addEventListener js/document "keyup" key-up false)
  (.addEventListener js/document "mousedown" mouse-down false)
  (.addEventListener js/document "mouseup" mouse-up false)
  (.addEventListener js/document "mousemove" mouse-move false)
  (.addEventListener js/window "resize" resize false))

(defn event-chan
  [c id el type data]
  (let [writer #(put! c [id % data])]
    (events/listen! el type writer)
    {:chan c
     :unsubscribe #(.removeEventListener el type writer)}))

(defn key-code
  [event]
  (.-keyCode (events/raw-event event)))

(defn dispatch-message
  [handlers]
  (go
   (while true
     (let [msg (<! receive)
           raw (.-data msg)
           data (reader/read-string raw)
           handler (get handlers (:op data))]
       (if handler
         (handler data)
         (log (str "op not supported! " data)))))))

(defn make-sender
  [senders]
  (log "HELLO")
  (event-chan send :click js/document.body :click {})
  (go
   (while true
     (let [[id event data] (<! send)
           sender (get senders id)]
       (if sender
         (sender event data))))))

(defn make-receiver 
  [receivers]
  (set! 
   (.-onmessage ws)
   (fn [msg]
     (put! receive msg)))
  (set!
   (.-onopen ws)
   (fn [msg] 
     (.send ws {:op :init})))
  (dispatch-message receivers))

(defn init-websockets
  [senders receivers]
  (make-sender senders)
  (make-receiver receivers))

