# HaskmeBot TODO list

Maybe we just create a DSL for commands so nothing is hard-coded.
This means basically a DSL for essentially (IrcEvent -> m Action)
This might also work with timers if timers is a thing handled by `m`.

```haskell
data IrcEvent
    = BotConnectedToServer -- join channel
    | ChannelMessage Channel User Text
    | PrivateMessage User Text -- maybe mods or other users want to interact somehow?
    -- | Kick/Ban/whatever -- might want to do something?

data Action
    | Reply Destination Text
```

```
addCommand : IrcEvent -> [Text] ~> Action
addCommand (ChanelMessage c u _) (name : rest) =
  useSomeLanguagePrimitiveToStoreDataInAMap name (fold rest)
  send $ Reply (Channel c) "Created command."
```

## Simple version of the language
- untyped
- not have constructors

each file is a command instead of parsing multiple commands in a file

```
-- this file is 'addCommand.hmb'
-- in chat: !addCommand !bot Hi this bot lives at github://...
-- then rest = ["Hi", "this", "bot", ...]
on Privmsg "#cvladfp" ("!addCommand":command:rest)

primStoreDataInAMap "commands" command (primFold rest)
primSend "#cvladfp" "Created command."

done

-----------------------------------

on Privmsg "#cvladfp" (command:[])

response = primGrabDataFromMap "commands" command
prinSendMaybe #cvladfp" response

done
```

```json
{ "data": {
    "commands": { "!bot": "text..." }
  }
}
```

