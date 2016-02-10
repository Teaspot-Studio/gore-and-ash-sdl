FROM phadej/ghc:7.10.2-ubuntu

RUN add-apt-repository ppa:zoogie/sdl2-snapshots
RUN apt-get update
RUN apt-get install -y pkg-config libsdl2-dev curl

ADD mkdocs /opt/project/
ADD LICENSE /opt/project/
ADD Setup.hs /opt/project/
ADD README.md /opt/project/
ADD gore-and-ash-sdl.cabal /opt/project/
ADD stack.yaml /opt/project/
ADD src /opt/project/src

WORKDIR /opt/project

ENTRYPOINT ["./mkdocs", "gore-and-ash-sdl", "2.0.0.0", "NCrashed"]