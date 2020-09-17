# haskmebot

## PLAN

- dynamically add, update, or modify the list of text-only commands

!mydotfiles -> "..."
!command add key text goes here...

1. Hold in-memory `Map` of commands
2. Have a case for the `!command` commands
3. Update the in-memory `Map`
4. Persist the in-memory `Map`

- read token from environment

- API-level commands, like:

!starttime <hour>
would do some
http post <url>/set-hour <hour>
