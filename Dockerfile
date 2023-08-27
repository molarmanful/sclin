# assumes assembled

FROM ubuntu as pre

RUN mkdir scbin
COPY out/sclin/assembly.dest/out.jar /scbin/sclin