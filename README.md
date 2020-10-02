# haskmebot

# Streaming software architecture

- Streamlabs OBS running the "scene" (UE4 overlay)
- The "scene" does http requests to the "overlay" backend (Purescript)
- This bot will send HTTP commands to the overlay in order to control the scene

## Done

- `!command add <key> <text ...>`
- `!key`
- persisting commands across runs
- read token from environment
    - re-generate oauth token
- !starttime <hour>:<minute>
- user roles
- recurrent messages in chat
- make our config files reasonable
- configure multiple repeated notification messages
- allow trusted users to update existing commands

## In-Progress


## PLAN



- save chat messages in the log as well

haskmebot = Haskell Twitch Bot
backend   = Purescript web server
frontend  = Unreal Engine 4 3D scene(s)

- bot features
    - some discord integration (live notification)
    - show info (today, next show, etc.)

- main scene
    - figure it out

- notifications (online, uptime, follow, subs, bits)
    - these need to be implemented in the backend

- random ideas
    - we can generate animations using a Haskell DSL:
        https://hackage.haskell.org/package/reanimate
            So for example, we could have a command like
            "!gen Hello world" and then we could generate
            a video with dancing text or whatever, then
            display it on stream.

            Or we could have the chat appear on screen
            as animated text using this library.



- API-level commands, like:
would do some
http post <url>/set-hour <hour>
