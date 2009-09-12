This is a simple gen_server node to send messages on AIM, Google Chat, Email, SMS, or Twitter using messagepub.

It is very easy to use:

1) Sign up for an account at <a href="http://messagepub.com">messagepub.com</a>

2) Once you've signed up, get your API key by going to your <em>Account Settings</em> at messagepub.com.

3) Compile messagepub.erl.  Don't forget to rr("messagepub.hrl") for records if in shell.

<pre>
c(messagepub).
rr("messagepub.hrl").
</pre>

4) Start the gen_server with your API KEY: 

<pre>
messagepub:start_link("YOUR API KEY").
</pre>

5) Build a list of recipients you wish to send to.  Order of recipients is used as the position in messagepub.

<pre>
Recipients = [messagepub:email_recipient("joe@example.com"), messagepub:sms_recipient("123456789")].
</pre>

Available helpers for building recipients are `twitter_recipient/1, gchat_recipient/1, aim_recipient/1, email_recipient/1, sms_recipient/1, phone_recipient/1`.

6) Build and send your notification:

<pre>
Notification = messagepub:new_notification("Your message here!", Recipients).
messagepub:create(Notification).
</pre>

Supported calls to api through: `create/1, view/0, get_notification/1, cancel/1, replies/0`

