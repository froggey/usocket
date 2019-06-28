;;;; -*- Mode: Common-Lisp -*-

;;;; See LICENSE for licensing information.

(in-package :usocket)

(defun handle-condition (condition &optional (socket nil))
  (typecase condition
    ;; TODO: Add additional conditions as appropriate
    ;; An error-handling function, resolving implementation specific errors
    ;; to this list of errors:
    ;;  - address-in-use-error
    ;;  - address-not-available-error
    ;;  - bad-file-descriptor-error
    ;;  - connection-refused-error
    ;;  - invalid-argument-error
    ;;  - no-buffers-error
    ;;  - operation-not-supported-error
    ;;  - operation-not-permitted-error
    ;;  - protocol-not-supported-error
    ;;  - socket-type-not-supported-error
    ;;  - network-unreachable-error
    ;;  - network-down-error
    ;;  - network-reset-error
    ;;  - host-down-error
    ;;  - host-unreachable-error
    ;;  - shutdown-error
    (mezzano.network.tcp:connection-timed-out
     (error 'timeout-error :socket socket))
    ;;  - unkown-error
    ;;  - interrupted-condition
    ;;  - unkown-condition
    ))

(defun socket-connect (host port &key (protocol :stream) element-type
			    timeout deadline (nodelay nil nodelay-p)
			    local-host local-port)
  (declare (ignore local-host local-port))
  (when deadline
    (unsupported 'deadline 'socket-connect))
  (when (and nodelay-p (not (eq nodelay :if-supported)))
    (unsupported 'nodelay 'socket-connect))
  (when timeout
    (unsupported 'timeout 'socket-connect))
  (with-mapped-conditions ()
    (ecase protocol
      (:stream
       (let ((s (mezzano.network.tcp:tcp-stream-connect host port :element-type element-type)))
         (make-stream-socket :socket s
                             :stream s)))
      (:datagram
       ;; TODO:
       (unsupported 'datagram 'socket-connect)))))

(defun socket-listen (host port &key reuseaddress
                                     (reuse-address nil reuse-address-supplied-p)
                                     (backlog 5)
                                     (element-type 'character))
  (declare (ignore reuseaddress reuse-address reuse-address-supplied-p))
  (let ((ip (mezzano.network.ip:make-ipv4-address host)))
    (make-stream-server-socket (mezzano.network.tcp:tcp-listen ip port :backlog backlog)
                               :element-type element-type)))

(defun get-hosts-by-name (name))

(defun get-host-by-address (address))

(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defun %add-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun %remove-waiter (wait-list waiter)
  (declare (ignore wait-list waiter)))

(defun wait-for-input-internal (wait-list &key timeout)
  (with-mapped-conditions ()
    (let ((waiters (wait-list-waiters wait-list)))
      (dolist (waiter waiters)
        (setf (state waiter)
              (if (mezzano.network.tcp::refill-tcp-packet-buffer (socket waiter))
                  :read
                  nil))))))

(defmethod socket-close ((usocket stream-usocket))
  (with-mapped-conditions ()
    (close (socket-stream usocket))))

(defmethod socket-close ((usocket stream-server-usocket))
  (with-mapped-conditions ()
    (mezzano.network.tcp:close-tcp-listener (socket usocket))))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (declare (ignore element-type))
  (with-mapped-conditions (usocket)
    (let ((s (mezzano.network.tcp:tcp-accept (socket usocket))))
      (make-stream-socket :socket s
                          :stream s))))

(defmethod get-local-name ((usocket usocket)))

(defmethod get-peer-name ((usocket stream-usocket)))

(defmethod get-local-address ((usocket usocket))
  (mezzano.network.ip::format-ipv4-address
   nil
   (mezzano.network.ip::ipv4-address-address
    (mezzano.network.tcp::tcp-connection-local-ip
     (mezzano.network.tcp::tcp-stream-connection (socket usocket))))))

(defmethod get-local-port ((usocket usocket))
  (mezzano.network.tcp::tcp-connection-local-port (mezzano.network.tcp::tcp-stream-connection (socket usocket))))

(defmethod get-peer-address ((usocket stream-usocket))
  (mezzano.network.ip::format-ipv4-address
   nil
   (mezzano.network.ip::ipv4-address-address
    (mezzano.network.tcp::tcp-connection-remote-ip
     (mezzano.network.tcp::tcp-stream-connection (socket usocket))))))

(defmethod get-peer-port ((usocket stream-usocket))
  (mezzano.network.tcp::tcp-connection-remote-port (mezzano.network.tcp::tcp-stream-connection (socket usocket))))
