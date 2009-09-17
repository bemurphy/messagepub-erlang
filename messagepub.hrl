-record(notification ,{
          body,
          subject,
          escalation,
          send_at,
          recipients
}).

-record(recipient, {
          channel,
          address
}).
