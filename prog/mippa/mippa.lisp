(defun mippa~send-request (mippamethod args)
  (when rpc*caller
    (socket~define :new)
    (socket~connect (first rpc*caller) (second rpc*caller) :new)
    (http~send-request :new (rpc~compose-methodcall mippamethod args) :uri (third rpc*caller))
    (socket~close :new)
    (socket~delete :new)))
