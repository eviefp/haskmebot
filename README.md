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

## In-Progress
- user roles

## PLAN

haskmebot = Haskell Twitch Bot
backend   = Purescript web server
frontend  = Unreal Engine 4 3D scene(s)

- notifications (online, uptime, follow, subs, bits)
    - implement them in haskmebot
    - have it send updates to the backend
    - backend send them to the frontend

- main scene
    - figure it out

- bot features
    - some discord integration (live notification)
    - show info (today, next show, etc.)

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
- use real data format instead of show/read
