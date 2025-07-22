FROM haskell:9.10.1 AS base

COPY libpq ./libpq/
RUN chmod +x ./libpq/install-libpq.sh
RUN sh ./libpq/install-libpq.sh

FROM base AS build
WORKDIR /servidor

COPY rinha2025-haskell.cabal ./
COPY CHANGELOG.md ./
COPY LICENSE ./
COPY app ./app/
COPY src ./src/

RUN cabal user-config update
RUN cabal update
RUN cabal install cabal-plan --constraint='cabal-plan +exe'

RUN cabal build
RUN mkdir ./bin
RUN cp $(cabal-plan list-bin rinha2025-haskell) ./bin

ARG HEAD_SERVER=1
ARG STAGE
COPY .env.$STAGE ./bin/.env
RUN cat ./bin/.env | sed "s/HEAD_SERVER=1/HEAD_SERVER=$HEAD_SERVER/" > ./bin/.env.tmp
RUN rm ./bin/.env
RUN mv ./bin/.env.tmp ./bin/.env

FROM base AS production
COPY --from=build /servidor/bin ./rinha2025-haskell/
WORKDIR /rinha2025-haskell
COPY --from=build /servidor/bin/.env ./.env

RUN mkdir ./src
RUN mkdir ./src/RepositoryLib
COPY src/RepositoryLib/*.sql ./src/RepositoryLib

EXPOSE 80

USER www-data:www-data
ENTRYPOINT ["./rinha2025-haskell"]
