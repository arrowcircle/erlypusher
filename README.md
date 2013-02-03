erlypusher
==========

[![Build Status](https://travis-ci.org/arrowcircle/erlypusher.png?branch=master)](https://travis-ci.org/arrowcircle/erlypusher)

Erlang server for pusher app


* Temp info

    def authentication_string(socket_id, custom_string = nil)
      if socket_id.nil? || socket_id.empty?
        raise Error, "Invalid socket_id #{socket_id}"
      end

      unless custom_string.nil? || custom_string.kind_of?(String)
        raise Error, 'Custom argument must be a string'
      end

      string_to_sign = [socket_id, name, custom_string].
        compact.map(&:to_s).join(':')
      Pusher.logger.debug "Signing #{string_to_sign}"
      token = @client.authentication_token
      digest = OpenSSL::Digest::SHA256.new
      signature = OpenSSL::HMAC.hexdigest(digest, token.secret, string_to_sign)

      return "#{token.key}:#{signature}"
    end
