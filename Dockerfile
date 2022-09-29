FROM haskell
WORKDIR /app
ADD . /app

RUN stack setup
RUN stack install yesod-bin --install-ghc
RUN stack build

ENTRYPOINT stack run
EXPOSE 3000
