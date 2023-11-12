# assumes assembled

FROM eclipse-temurin:21-jre as pre

RUN mkdir scbin
COPY out/sclin/assembly.dest/out.jar /scbin/sclin
