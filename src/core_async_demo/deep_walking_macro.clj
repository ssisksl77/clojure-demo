(ns core-async-demo.deep-walking-macro)

(defmulti parse-item (fn [form ctx]
                       (cond
                         (seq? form) :seq
                         (integer? form) :int
                         (symbol? form) :symbol
                         (nil? form) :nil
                         (boolean? form) :b)))

(defmulti parse-sexpr (fn [[sym & rest] ctx]
                        sym))

(defmethod parse-sexpr 'if
  [[_ test then else] ctx]
  {:type :if
   :test (parse-item test ctx)
   :then (parse-item then ctx)
   :else (parse-item else ctx)})

(defmethod parse-sexpr 'do
  [[_ & body] ctx]
  {:type :do
   :body (doall (map (fn [x] (parse-item x ctx))
                     body))})

(defmethod parse-sexpr :default
  [[f & body] ctx]
  {:type :call
   :fn (parse-item f ctx)
   :args (doall (map (fn [x] (parse-item x ctx))
                     body))})

(defmethod parse-item :seq
  [form ctx]
  (let [form (macroexpand form)]
    (parse-sexpr form ctx)))

(defmethod parse-item :int
  [form ctx]
  (swap! ctx inc)
  {:type :int
   :value form})

(defmethod parse-item :symbol
  [form ctx]
  {:type :symbol
   :value form})
(defmethod parse-item :b
  [form ctx]
  {:type :boolean
   :value form})

(defmethod parse-item :nil
  [form ctx]
  {:type :nil})

(defmacro to-ast [form]
  (pr-str (parse-item form (atom 0))))


(to-ast (+ 1 2))
(to-ast (if true 1 2))
(if true
  1
  2)
;;;;;;;;;;;;;;;;; currying intro
(def add
  (fn [x]
    (fn [y]
      (+ x y))))
(add 1)
((add 1) 1)

(def incer (partial + 1))
(incer 5)
;; haskell, ML, etc...
;; put operations into hashmap but not execute now. we will execute later on.
(defn r-assoc [k v m]
  (assoc m k v))

(def add-me (partial r-assoc :name "Younghwan"))
(add-me {})
;; why this is iteresting.
;; ability. takes a hashmap and return a hashmap (monad)
;; and chain it.

;; threading macro. 매크로끼리 연결.
(defn thread-it [& fns]
           (reduce
            (fn [acc f]
              (f acc))
            {}
            fns))

(thread-it (partial r-assoc :name "Younghwan")
           (partial r-assoc :last-name "Nam")
           (partial r-assoc :age 1))
;; {} is hard coded. this is not composable.

(defn thread-it-2 [& fns]
  (fn [initial]
    (reduce
     (fn [acc f]
       (f acc))
     initial
     fns)))

(thread-it-2 (partial r-assoc :name "Younghwan")
             (partial r-assoc :last-name "NAM")
             (partial r-assoc :age 1))
;; currying and attach {}

((thread-it-2 (partial r-assoc :name "YH")
              (partial r-assoc :last-name "NAM")
              (partial r-assoc :age 1))
 {})
;; now we can compose it
(defn add-me-info []
  (thread-it-2 (partial r-assoc :name "YH")
               (partial r-assoc :last-name "NAM")
               (partial r-assoc :age 1)))
(defn add-me-job []
  (thread-it-2 (partial r-assoc :job "Developer")))

((thread-it-2 (add-me-info)
              (add-me-job))
 {})
;; **
;; this is a monad. doesn't matter what it is.
;; without having to worry about mutablity. it allows me to write analyzers and compiler in a functionally pure way.

;; 맨 위에 있는 parse-sexpr를 보면 thread-it과 아주 비슷한 일을 한다. 매크로까지 더해져서
;; 우리는 코드의 컨텍스트까지 분석해서 어떤 일을 연결시킨다.


;;;; core.async source code
(defmacro gen-plan
  "Allows a user to define a state monad binding plan.
  (gen-plan
   [_ (assoc-in-plan [:foo :bar] 42)
    val (get-in-plan [:foo :bar])]
   val)"
  [binds id-expr]
  (let [binds (partition 2 binds)
        psym (gensym "plan_")
        forms (reduce
               (fn [acc [id expr]]
                 (concat acc `[[~id ~psym] (~expr ~psym)]))
               []
               binds)]  ;; 여러개의 일들을 합치는 것이다. 
    `(fn [~psym]
       (let [~@forms]
         [~id-expr ~psym]))))  ;; 그것을 하나의 함수로 변경하는 듯.

(defn assoc-in-plan
  "Same as assoc-in, but for state hash map"
  [path val]
  (fn [plan]
    [val (assoc-in plan path val)]))

(assoc-in-plan [:test] "Hello")

((assoc-in-plan [:test] "Hello") {})

(gen-plan
 [assoc-v (assoc-in-plan [:test] "Helloo")]  ;; id="Helloo
 (str "value is " assoc-v))  ;; 
;; return function
((gen-plan
  [assoc-v (assoc-in-plan [:test] "Hello")]
  (str "value is " assoc-v))
 {})
;; numberint test

(comment
  ((gen-plan
    (assoc-v (for [x (range 5)]
               (assoc-in-plan [:text] "Hello")))
    (str "value is " assoc-v))
   {}))
;; clojure.lang.LazySeq cannot be cast to clojure.lang.Ifn
;; for is lazy 가 함수로 바껴야 한다.
(defn all
  "Assumes that itms is a list of state monad function results, threads the state map
  through all of them. Returns a vector of all the results."
  [itms]
  (fn [plan]
    (reduce
     (fn [[ids plan] f]
       (let [[id plan] (f plan)]
         [(conj ids id) plan]))
     [[] plan]
     itms)))

(all (for [x (range 5)]
       (assoc-in-plan [:test] "Hello")))

((all (for [x (range 5)]
        (assoc-in-plan [:text x] (str "Hello " x))))
 {})

((gen-plan
  [assoc-v (all (for [x (range 5)]
                  (assoc-in-plan [:test x] (str "Hello " x))))]
  (str "value is " assoc-v))
 {})

;; get-plan get a plan made by gen-plan
(defn get-plan
  "Returns the final [id state] from a plan. "
  [f]
  (f {}))

(get-plan
 (gen-plan
  [assoc-v (all (for [x (range 5)]
                  (assoc-in-plan [:test x] (str "Hello " x))))]
  (str "value is " assoc-v)))
;; 그냥 위에 한 거랑 똑같은 거임.

(defn push-binding
  "Sets the binding 'key' to value. This operation can be undone via pop-bindings.
  Bindings are stored in the state hashmap."
  [key value]
  (fn [plan]
    [nil (update-in plan [:bindings key] conj value)])) ;; :bindings안에 key안에 값을 추가한다.

(defn get-binding
  "Gets the value of the curent binding for key"
  [key]
  (fn [plan]
    [(first (get-in plan [:bindings key])) plan]))  ;; bindings key 안에 있는 값을 빼낸다.

(defn pop-binding
  "Removes the most recent binding for key"
  [key]
  (fn [plan]
    [(first (get-in plan [:bindings key]))
     (update-in plan [:bindings key] pop)]))  ;; 빼낸다.
;; not mutate, work with a context (monad)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go macro
;; 1. parse code into a SSA format
;; 2. put it all back into clojure code.
(comment
  (defn state-machine [body num-user-params env user-transitions]
    (-> (parse-to-state-machine body env user-transitions)
        second
        (emit-state-machine num-user-params user-transitions))))

;; (parse-to-state-machine body env user-transitions)
;; 해당 함수를 추가해보자.
(defn push-binding
  "Sets the binding 'key' to value. This operation can be undone via pop-bindings.
   Bindings are stored in the state hashmap."
  [key value]
  (fn [plan]
    [nil (update-in plan [:bindings key] conj value)]))
(defn update-in-plan
  "Same as update-in, but for a state hash map"
  [path f & args]
  (fn [plan]
    [nil (apply update-in plan path f args)]))
(defn get-in-plan
  "Same as get-in, but for a state hash map"
  [path]
  (fn [plan]
    [(get-in plan path) plan]))
(defn get-block
  "Gets the current block"
  []
  (fn [plan]
    [(:current-block plan) plan]))
(defn no-op
  "This function can be used inside a gen-plan when no operation is to be performed"
  []
  (fn [plan]
    [nil plan]))
(defn add-block
  "Adds a new block, returns its id, but does not change the current block (does not call set-block)."
  []
  (gen-plan
   [_ (update-in-plan [:block-id] (fnil inc 0))
    blk-id (get-in-plan [:block-id])
    cur-blk (get-block)
    _ (assoc-in-plan [:blocks blk-id] [])
    catches (get-binding :catch)
    _ (assoc-in-plan [:block-catches blk-id] catches)
    _ (if-not cur-blk
        (assoc-in-plan [:start-block] blk-id)
        (no-op))]
   blk-id))
(defn set-block
  "Sets the current block being written to by the functions. The next add-instruction call will append to this block"
  [block-id]
  (fn [plan]
    [block-id (assoc plan :current-block block-id)]))
;;;; parsing and into ssa
;; Dispatch clojure forms based on :op
(def -item-to-ssa nil) ;; for help in the repl
(defmulti -item-to-ssa :op)
(defprotocol IEmittableInstruction
  (emit-instruction [this state-sym] "Returns the clojure code that this instruction represents"))
(defprotocol IInstruction
  (reads-from [this] "Returns a list of instructions this instruction reads from")
  (writes-to [this] "Returns a list of instructions this instruction writes to")
  (block-references [this] "Returns all the blocks this instruction references"))
(defrecord RawCode [ast locals]
  IInstruction
  (reads-from [this]
    (keep (or locals #{})
          (map :name (-> ast :env :locals vals))))
  (writes-to [this] [(:id this)])
  (block-references [this] [])
  IEmittableInstruction
  (emit-instruction [this state-sym]
    (if (not-empty (reads-from this))
      `[~@(->> (-> ast :env :locals vals)
               (map #(select-keys % [:op :name :form]))
               (filter (fn [local]
                         (when locals
                           (get locals (:name local)))))
               set
               (mapcat
                (fn [local]
                  `[~(:form local) ~(get locals (:name local))]))) 
        ~(:id this) ~(:form ast)]
      `[~(:id this) ~(:form ast)])))

(defn add-instruction
  "Appends an instruction to the current block. "
  [inst]
  (let [inst-id (with-meta (gensym "inst_")
                  {::instruction true})
        inst (assoc inst :id inst-id)]
    (gen-plan
     [blk-id (get-block)
      _ (update-in-plan [:blocks blk-id] (fnil conj []) inst)]
     inst-id)))
(defmethod -item-to-ssa :default
  [ast]
  (gen-plan
   [locals (get-binding :locals)
    id (add-instruction (->RawCode ast locals))]
   id))

(defn item-to-ssa [ast]
  (if (or (::transform? ast)
          (contains? #{:local :const :quote} (:op ast)))
    (-item-to-ssa ast)
    (gen-plan
     [locals (get-binding :locals)
      id (add-instruction (->RawCode ast locals))]
     id)))
(defrecord Call [refs]
  IInstruction
  (reads-from [this] refs)
  (writes-to [this] [(:id this)])
  (block-references [this] [])
  IEmittableInstruction
  (emit-instruction [this state-sym]
    `[~(:id this) ~(seq refs)]))
(defmethod -item-to-ssa :invoke
  [{f :fn args :args}]
  (gen-plan
   [arg-ids (all (map item-to-ssa (cons f args)))
    inst-id (add-instruction (->Call arg-ids))]
   inst-id))

(defmethod -item-to-ssa :keyword-invoke
  [{f :keyword target :target}]
  (gen-plan
   [arg-ids (all (map item-to-ssa (list f target)))
    inst-id (add-instruction (->Call arg-ids))]
   inst-id))

(defmethod -item-to-ssa :protocol-invoke
  [{f :protocol-fn target :target args :args}]
  (gen-plan
   [arg-ids (all (map item-to-ssa (list* f target args)))
    inst-id (add-instruction (->Call arg-ids))]
   inst-id))

(defmethod -item-to-ssa :instance?
  [{:keys [class target]}]
  (gen-plan
   [arg-id (item-to-ssa target)
    inst-id (add-instruction (->Call (list `instance? class arg-id)))]
   inst-id))

(defmethod -item-to-ssa :prim-invoke
  [{f :fn args :args}]
  (gen-plan
   [arg-ids (all (map item-to-ssa (cons f args)))
    inst-id (add-instruction (->Call arg-ids))]
   inst-id))
(defrecord InstanceInterop [instance-id op refs]
  IInstruction
  (reads-from [this] (cons instance-id refs))
  (writes-to [this] [(:id this)])
  (block-references [this] [])
  IEmittableInstruction
  (emit-instruction [this state-sym]
    `[~(:id this) (. ~instance-id ~op ~@(seq refs))]))
(defmethod -item-to-ssa :instance-call
  [{:keys [instance method args]}]
  (gen-plan
   [arg-ids (all (map item-to-ssa args))
    instance-id (item-to-ssa instance)
    inst-id (add-instruction (->InstanceInterop instance-id method arg-ids))]
   inst-id))

(defmethod -item-to-ssa :instance-field
  [{:keys [instance field]}]
  (gen-plan
   [instance-id (item-to-ssa instance)
    inst-id (add-instruction (->InstanceInterop instance-id (symbol (str "-" field)) ()))]
   inst-id))

(defmethod -item-to-ssa :host-interop
  [{:keys [target m-or-f]}]
  (gen-plan
   [instance-id (item-to-ssa target)
    inst-id (add-instruction (->InstanceInterop instance-id m-or-f ()))]
   inst-id))
(defrecord StaticCall [class method refs]
  IInstruction
  (reads-from [this] refs)
  (writes-to [this] [(:id this)])
  (block-references [this] [])
  IEmittableInstruction
  (emit-instruction [this state-sym]
    `[~(:id this) (. ~class ~method ~@(seq refs))]))

(defmethod -item-to-ssa :static-call
  [{:keys [class method args]}]
  (gen-plan
   [arg-ids (all (map item-to-ssa args))
    inst-id (add-instruction (->StaticCall class method arg-ids))]
   inst-id))

(defmethod -item-to-ssa :set!
  [{:keys [val target]}]
  (gen-plan
   [arg-ids (all (map item-to-ssa (list target val)))
    inst-id (add-instruction (->Call (cons 'set! arg-ids)))]
   inst-id))

(defn var-name [v]
  (let [nm (:name (meta v))
        nsp (.getName ^clojure.lang.Namespace (:ns (meta v)))]
    (symbol (name nsp) (name nm))))


(defmethod -item-to-ssa :var
  [{:keys [var]}]
  (gen-plan
   []
   (var-name var)))

(defmethod -item-to-ssa :const
  [{:keys [form]}]
  (gen-plan
   []
   form))
(defn push-alter-binding
  "Pushes the result of (apply f old-value args) as current value of binding key"
  [key f & args]
  (fn [plan]
    [nil (update-in plan [:bindings key]
                    #(conj % (apply f (first %) args)))]))
(defn let-binding-to-ssa
  [{:keys [name init form]}]
  (gen-plan
   [bind-id (item-to-ssa init)
    _ (push-alter-binding :locals assoc (vary-meta name merge (meta form)) bind-id)]
   bind-id))
(def ^:const FN-IDX 0)
(def ^:const STATE-IDX 1)
(def ^:const VALUE-IDX 2)
(def ^:const BINDINGS-IDX 3)
(def ^:const EXCEPTION-FRAMES 4)
(def ^:const CURRENT-EXCEPTION 5)
(def ^:const USER-START-IDX 6)
(defrecord Const [value]
  IInstruction
  (reads-from [this] [value])
  (writes-to [this] [(:id this)])
  (block-references [this] [])
  IEmittableInstruction
  (emit-instruction [this state-sym]
    (if (= value ::value)
      `[~(:id this) (aget-object ~state-sym ~VALUE-IDX)]
      `[~(:id this) ~value])))
(defmethod -item-to-ssa :let
  [{:keys [bindings body]}]
  (gen-plan
   [let-ids (all (map let-binding-to-ssa bindings))
    _ (all (map (fn [_] (pop-binding :locals)) bindings))

    local-ids (all (map (comp add-instruction ->Const) let-ids))
    _ (push-alter-binding :locals merge (into {} (map (fn [id {:keys [name form]}]
                                                        [name (vary-meta id merge (meta form))])
                                                      local-ids bindings)))

    body-id (item-to-ssa body)
    _ (pop-binding :locals)]
   body-id))
(defprotocol ITerminator
  (terminator-code [this] "Returns a unique symbol for this instruction")
  (terminate-block [this state-sym custom-terminators] "Emites the code to terminate a given block"))
(defrecord Jmp [value block]
  IInstruction
  (reads-from [this] [value])
  (writes-to [this] [])
  (block-references [this] [block])
  ITerminator
  (terminate-block [this state-sym _]
    `(do (aset-all! ~state-sym ~VALUE-IDX ~value ~STATE-IDX ~block)
         :recur)))
(defmethod -item-to-ssa :loop
  [{:keys [body bindings] :as ast}]
  (gen-plan
   [local-val-ids (all (map let-binding-to-ssa bindings))
    _ (all (for [_ bindings]
             (pop-binding :locals)))
    local-ids (all (map (comp add-instruction ->Const) local-val-ids))
    body-blk (add-block)
    final-blk (add-block)
    _ (add-instruction (->Jmp nil body-blk))

    _ (set-block body-blk)
    _ (push-alter-binding :locals merge (into {} (map (fn [id {:keys [name form]}]
                                                        [name (vary-meta id merge (meta form))])
                                                      local-ids bindings)))
    _ (push-binding :recur-point body-blk)
    _ (push-binding :recur-nodes local-ids)

    ret-id (item-to-ssa body)

    _ (pop-binding :recur-nodes)
    _ (pop-binding :recur-point)
    _ (pop-binding :locals)
    _ (if (not= ret-id ::terminated)
        (add-instruction (->Jmp ret-id final-blk))
        (no-op))
    _ (set-block final-blk)
    ret-id (add-instruction (->Const ::value))]
   ret-id))

(defmethod -item-to-ssa :do
  [{:keys [statements ret] :as ast}]
  (gen-plan
   [_ (all (map item-to-ssa statements))
    ret-id (item-to-ssa ret)]
   ret-id))
(defrecord Case [val-id test-vals jmp-blocks default-block]
  IInstruction
  (reads-from [this] [val-id])
  (writes-to [this] [])
  (block-references [this] [])
  ITerminator
  (terminate-block [this state-sym _]
    `(do (case ~val-id
           ~@(concat (mapcat (fn [test blk]
                               `[~test (aset-all! ~state-sym
                                                  ~STATE-IDX ~blk)])
                             test-vals jmp-blocks)
                     (when default-block
                       `[(do (aset-all! ~state-sym ~STATE-IDX ~default-block)
                             :recur)])))
         :recur)))
(defmethod -item-to-ssa :case
  [{:keys [test tests thens default] :as ast}]
  (gen-plan
   [end-blk (add-block)
    start-blk (get-block)
    clause-blocks (all (map (fn [expr]
                              (assert expr)
                              (gen-plan
                               [blk-id (add-block)
                                _ (set-block blk-id)
                                expr-id (item-to-ssa expr)
                                _ (if (not= expr-id ::terminated)
                                    (add-instruction (->Jmp expr-id end-blk))
                                    (no-op))]
                               blk-id))
                            (map :then thens)))
    default-block (if default
                    (gen-plan
                     [blk-id (add-block)
                      _ (set-block blk-id)
                      expr-id (item-to-ssa default)
                      _ (if (not= expr-id ::terminated)
                          (add-instruction (->Jmp expr-id end-blk))
                          (no-op))]
                     blk-id)
                    (no-op))
    _ (set-block start-blk)
    val-id (item-to-ssa test)
    case-id (add-instruction (->Case val-id (map (comp :form :test) tests)
                                     clause-blocks
                                     default-block))
    _ (set-block end-blk)
    ret-id (add-instruction (->Const ::value))]
   ret-id))

(defmethod -item-to-ssa :quote
  [{:keys [form]}]
  (gen-plan
   [ret-id (add-instruction (->Const form))]
   ret-id))
(defrecord EndFinally []
  IInstruction
  (reads-from [this] [])
  (writes-to [this] [])
  (block-references [this] [])
  IEmittableInstruction
  (emit-instruction [this state-sym]
    `[~'_ (when-let [e# (aget-object ~state-sym ~CURRENT-EXCEPTION)]
            (throw e#))]))

(defrecord CatchHandler [catches]
  IInstruction
  (reads-from [this] [])
  (writes-to [this] [])
  (block-references [this] (map first catches))
  ITerminator
  (terminate-block [this state-sym _]
    (let [ex (gensym 'ex)]
      `(let [~ex (aget-object ~state-sym ~VALUE-IDX)]
         (aset-all! ~state-sym ~CURRENT-EXCEPTION ~ex)
         (cond
           ~@(for [[handler-idx type] catches
                   i [`(instance? ~type ~ex) ` (aset-all! ~state-sym
                                                          ~STATE-IDX ~handler-idx
                                                          ~CURRENT-EXCEPTION nil)]]
               i)
           :else (throw ~ex))
         :recur))))
(defrecord PushTry [catch-block]
  IInstruction
  (reads-from [this] [])
  (writes-to [this] [])
  (block-references [this] [catch-block])
  IEmittableInstruction
  (emit-instruction [this state-sym]
    `[~'_ (aset-all! ~state-sym ~EXCEPTION-FRAMES (cons ~catch-block (aget-object ~state-sym ~EXCEPTION-FRAMES)))]))

(defrecord PopTry []
  IInstruction
  (reads-from [this] [])
  (writes-to [this] [])
  (block-references [this] [])
  IEmittableInstruction
  (emit-instruction [this state-sym]
    `[~'_ (aset-all! ~state-sym ~EXCEPTION-FRAMES (rest (aget-object ~state-sym ~EXCEPTION-FRAMES)))]))
(defmethod -item-to-ssa :try
  [{:keys [catches body finally] :as ast}]
  (gen-plan
   [body-block (add-block)
    exit-block (add-block)
    ;; Two routes to the finally block, via normal execution and
    ;; exception execution
    finally-blk (if finally
                    (gen-plan
                     [cur-blk (get-block)
                      finally-blk (add-block)
                      _ (set-block finally-blk)
                      result-id (add-instruction (->Const ::value))
                      _ (item-to-ssa finally)
                      ;; rethrow exception on exception path
                      _ (add-instruction (->EndFinally))
                      _ (add-instruction (->Jmp result-id exit-block))
                      _ (set-block cur-blk)]
                     finally-blk)
                    (gen-plan [] exit-block))
    catch-blocks (all
                  (for [{ex-bind :local {ex :val} :class catch-body :body} catches]
                    (gen-plan
                     [cur-blk (get-block)
                      catch-blk (add-block)
                      _ (set-block catch-blk)
                      ex-id (add-instruction (->Const ::value))
                      _ (push-alter-binding :locals assoc (:name ex-bind)
                                            (vary-meta ex-id merge (when (:tag ex-bind)
                                                                     {:tag (.getName ^Class (:tag ex-bind))})))
                      result-id (item-to-ssa catch-body)
                      ;; if there is a finally, jump to it after
                      ;; handling the exception, if not jump to exit
                      _ (add-instruction (->Jmp result-id finally-blk))
                      _ (pop-binding :locals)
                      _ (set-block cur-blk)]
                     [catch-blk ex])))
    ;; catch block handler routes exceptions to the correct handler,
    ;; rethrows if there is no match
    catch-handler-block (add-block)
    cur-blk (get-block)
    _ (set-block catch-handler-block)
    _ (add-instruction (->CatchHandler catch-blocks))
    _ (set-block cur-blk)
    _ (add-instruction (->Jmp nil body-block))
    _ (set-block body-block)
    ;; the finally gets pushed on to the exception handler stack, so
    ;; it will be executed if there is an exception
    _ (if finally
        (add-instruction (->PushTry finally-blk))
        (no-op))
    _ (add-instruction (->PushTry catch-handler-block))
    body (item-to-ssa body)
    _ (add-instruction (->PopTry))
    _ (if finally
        (add-instruction (->PopTry))
        (no-op))
    ;; if the body finishes executing normally, jump to the finally
    ;; block, if it exists
    _ (add-instruction (->Jmp body finally-blk))
    _ (set-block exit-block)
    ret (add-instruction (->Const ::value))]
   ret))

(defmethod -item-to-ssa :throw
  [{:keys [exception] :as ast}]
  (gen-plan
   [exception-id (item-to-ssa exception)
    ret-id (add-instruction (->Call ['throw exception-id]))]
   ret-id))

(defmethod -item-to-ssa :new
  [{:keys [args class] :as ast}]
  (gen-plan
   [arg-ids (all (map item-to-ssa args))
    ret-id (add-instruction (->Call (list* 'new (:val class) arg-ids)))]
   ret-id))
(require '[clojure.set :refer (intersection union difference)])
(defn- emit-clashing-binds
  [recur-nodes ids clashes]
  (let [temp-binds (reduce
                    (fn [acc i]
                      (assoc acc i (gensym "tmp")))
                    {} clashes)]
    (concat
     (mapcat (fn [i]
               `[~(temp-binds i) ~i])
             clashes)
     (mapcat (fn [node id]
               `[~node ~(get temp-binds id id)])
             recur-nodes
             ids))))
(defrecord Recur [recur-nodes ids]
  IInstruction
  (reads-from [this] ids)
  (writes-to [this] recur-nodes)
  (block-references [this] [])
  IEmittableInstruction
  (emit-instruction [this state-sym]
    (if-let [overlap (seq (intersection (set recur-nodes) (set ids)))]
      (emit-clashing-binds recur-nodes ids overlap)
      (mapcat (fn [r i]
                `[~r ~i]) recur-nodes ids))))
(defmethod -item-to-ssa :recur
  [{:keys [exprs] :as ast}]
  (gen-plan
   [val-ids (all (map item-to-ssa exprs))
    recurs (get-binding :recur-nodes)
    _ (do (assert (= (count val-ids)
                     (count recurs))
                  "Wrong number of arguments to recur")
          (no-op))
    _ (add-instruction (->Recur recurs val-ids))

    recur-point (get-binding :recur-point)

    _ (add-instruction (->Jmp nil recur-point))]
   ::terminated))
(defrecord CondBr [test then-block else-block]
  IInstruction
  (reads-from [this] [test])
  (writes-to [this] [])
  (block-references [this] [then-block else-block])
  ITerminator
  (terminate-block [this state-sym _]
    `(do (if ~test
           (aset-all! ~state-sym
                      ~STATE-IDX ~then-block)
           (aset-all! ~state-sym
                      ~STATE-IDX ~else-block))
         :recur)))
(defmethod -item-to-ssa :if
  [{:keys [test then else]}]
  (gen-plan
   [test-id (item-to-ssa test)
    then-blk (add-block)
    else-blk (add-block)
    final-blk (add-block)
    _ (add-instruction (->CondBr test-id then-blk else-blk))

    _ (set-block then-blk)
    then-id (item-to-ssa then)
    _ (if (not= then-id ::terminated)
        (gen-plan
         [_ (add-instruction (->Jmp then-id final-blk))]
         then-id)
        (no-op))

    _ (set-block else-blk)
    else-id (item-to-ssa else)
    _ (if (not= else-id ::terminated)
        (gen-plan
         [_ (add-instruction (->Jmp else-id final-blk))]
         then-id)
        (no-op))

    _ (set-block final-blk)
    val-id (add-instruction (->Const ::value))]
   val-id))
(defrecord CustomTerminator [f blk values meta]
  IInstruction
  (reads-from [this] values)
  (writes-to [this] [])
  (block-references [this] [])
  ITerminator
  (terminate-block [this state-sym _]
    (with-meta `(~f ~state-sym ~blk ~@values)
      meta)))

(defmethod -item-to-ssa :transition
  [{:keys [name args form]}]
  (gen-plan
   [blk (add-block)
    vals (all (map item-to-ssa args))
    val (add-instruction (->CustomTerminator name blk vals (meta form)))
    _ (set-block blk)
    res (add-instruction (->Const ::value))]
   res))

(defmethod -item-to-ssa :local
  [{:keys [name form]}]
  (gen-plan
   [locals (get-binding :locals)
    inst-id (if (contains? locals name)
              (fn [p]
                [(locals name) p])
              (fn [p]
                [form p]))]
   inst-id))

(defmethod -item-to-ssa :map
  [{:keys [keys vals]}]
  (gen-plan
   [keys-ids (all (map item-to-ssa keys))
    vals-ids (all (map item-to-ssa vals))
    id (add-instruction (->Call (cons 'clojure.core/hash-map
                             (interleave keys-ids vals-ids))))]
   id))

(defmethod -item-to-ssa :with-meta
  [{:keys [expr meta]}]
  (gen-plan
   [meta-id (item-to-ssa meta)
    expr-id (item-to-ssa expr)
    id (add-instruction (->Call (list 'clojure.core/with-meta expr-id meta-id)))]
   id))

(defmethod -item-to-ssa :record
  [x]
  (-item-to-ssa `(~(symbol (.getName (class x)) "create")
                  (hash-map ~@(mapcat identity x)))))

(defmethod -item-to-ssa :vector
  [{:keys [items]}]
  (gen-plan
   [item-ids (all (map item-to-ssa items))
    id (add-instruction (->Call (cons 'clojure.core/vector
                                      item-ids)))]
   id))

(defmethod -item-to-ssa :set
  [{:keys [items]}]
  (gen-plan
   [item-ids (all (map item-to-ssa items))
    id (add-instruction (->Call (cons 'clojure.core/hash-set
                                      item-ids)))]
   id))
(defrecord Return [value]
  IInstruction
  (reads-from [this] [value])
  (writes-to [this] [])
  (block-references [this] [])
  ITerminator
  (terminator-code [this] :Return)
  (terminate-block [this state-sym custom-terminators]
    (if-let [f (get custom-terminators (terminator-code this))]
      `(~f ~state-sym ~value)
      `(do (aset-all! ~state-sym
                      ~VALUE-IDX ~value
                      ~STATE-IDX ::finished)
           nil))))

(defn parse-to-state-machine
  "Takes an sexpr and returns a hashmap that describes the execution flow of the sexpr as
   a series of SSA style blocks."
  [body terminators]
  (-> (gen-plan
       [_ (push-binding :terminators terminators)
        blk (add-block)
        _ (set-block blk)
        id (item-to-ssa body)
        term-id (add-instruction (->Return id))
        _ (pop-binding :terminators)]
       term-id)
      get-plan))



;;;;  하 드디어 우리가 사용할 녀석이 보임...
(defn parse-to-state-machine
  "Takes an sexpr and returns a hashmap that describes the execution flow of the sexpr as
   a series of SSA style blocks."
  [body terminators]
  (-> (gen-plan
       [_ (push-binding :terminators terminators)
        blk (add-block)
        _ (set-block blk)
        id (item-to-ssa body)
        term-id (add-instruction (->Return id))
        _ (pop-binding :terminators)]
       term-id)
      get-plan))
;; 테스트 해보자.
(-> (parse-to-state-machine '[(if (= x 1)
                                :true
                                :false)] {} )
    (clojure.pprint/pprint))
;; {1 {id1 (= x 1)
;;     condjmp id1 2 3}  cond인데 jump하는 거다 고투 같은 거임. 트루이면 2구역으로 아니면 3구역으로
;;  2 {jmp 4 :value true} 4구역으로 점프하고 :value true를 같이 보낸다.
;;  3 {jmp 4 :value false} 4구역으로 점프하고 :value false를 같이 보낸다.
;;  4 {return :value}}
;; 이게 말하는 게 머냐면, 꽤나 쉽다는 거다. 우리는 코드들을 이렇게 정리할 수 있는 것이다.
;; core.async
;; 아래 코드를 설명해보자.
;; #1
;; x = 0
;; jmp #2

;; #2
;; x1 = phi {#1 x
;            #5 x2}
;;               phi = where you come from , #1에서 왔으면 x, #5에서 왔으면 x2
;; cmpv = cmp x1 5      비교
;; condjmp compv #3 #4

;; #3
;; return :done

;; #4
;; put!-term 5 c x1

;; #5
;; x2 = inc x1
;; jmp #2

;; 이제 분석하는게 꽤나 쉽다.
;; 우리가 계속 트래킹 해야 하는 정보가 뭔가
;; [x x1 x2] 변수들이다.
(comment (loop [x 0]
           (if (= x 5)
             :done
             (do (>! c x)
                 (recur (inc))))))

(comment
  (loop []
    (let [result
          (case (:block-id state)
            1 (do (aset state :x 0)
                  (aset satte :block-id 2)
                  :recur)
            2 ("....blcok 2....." ))]
      (if (identical? result :recur)
        (recur)
        result))))


(-> (parse-to-state-machine '[(loop [x 0]
                                (if (= x 5)
                                  :done
                                  (do (>! c x)
                                      (recur (inc)))))] {} )
    (clojure.pprint/pprint))
;; monad 어떻게 가던 서울로만 가면됨. 어떻게 함수를 조합하던, 들어온 타입으로 똑같이 뱉어지도록 compose되면 됨.
