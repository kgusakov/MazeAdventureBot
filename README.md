![Scala CI](https://github.com/kgusakov/MazeAdventureBot/workflows/Scala%20CI/badge.svg?branch=master&event=push)

# MazeAdventureBot
A telegram bot, which implements simple version of https://en.wikipedia.org/wiki/Labyrinth_(paper-and-pencil_game)

## Requirements
- JDK 1.8
- SBT 0.13

## Build
```
sbt assembly
```

## Run
```
java -jar bot/target/scala-2.11/bot-assembly-1.0.jar <your_bot_key>
```

Note: current version of bot is working only in non-supergroup chats

## Contributors
- [@AlexeyNikolaev](https://github.com/AlexeyNikolaev) - the author of maze generator algorithm
