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

-record(reply, {
          id,
          notification_id,
          channel,
          address,
          body,
          mp3_url, %%hmmm, rarely used, what to do
          created_at
}).
