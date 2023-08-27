# assumes assembled

FROM eclipse-temurin:20-jre as pre

RUN mkdir scbin
COPY out/sclin/assembly.dest/out.jar /scbin/sclin