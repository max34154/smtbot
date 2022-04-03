(ns smtbot.bot.messages.tmessage)
(defprotocol TMessage
  " 
   Telegram message object handling 
  "
  (^String get-item-key [this] "get item key to select value from cache or target system")
  (^String get-query [this] "get query to select item ")
  (^String get-callback [this] "get callback id")
  (^String get-smkey [this] "get email of the user who placed request or 
                             nil if request does not need authorization")
  (get-role [this] "return role keyword only if role required access-list check e.g. contact, Otherwise nil.")
  (^String  get-user-mail [this])
  (^String  get-operator-id [this])
  (response [this sm-responce] "send response to telegram ")
  (error-response [this error-code] "send message about problem with getting info")
  (wait-a-minute [this] "send wait a minute response to telegram and return message object with wait-a-minute response id "))

(defn get-role-val
  "
   Return access-list val for specific role:
    - :contact -- user email
    - :operator --- ??? possible login 
   "
  [obj] ^String
  (case (obj :role)
    :contact  (obj :smkey)
    :operator (obj :login)
    nil))


#_(defn  get-user-mail [obj] ^String (obj :user-smkey))

#_(defn get-operator-id [obj] ^String (obj :user-smkey))


#_(defn get-role
    "
  Return role keyword only if role required access-list check e.g. contact, 
  Otherwise nil.
  "
 ;!!! Suppose that check was done by smtbot.bot.users.operations/set-role
    [obj]
    (obj :user-role))