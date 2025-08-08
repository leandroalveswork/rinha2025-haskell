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

RUN mkdir -p ./scripts
COPY scripts/*.sql ./scripts/
COPY scripts/env_replacement.sh ./scripts/
RUN chmod +x ./scripts/env_replacement.sh


RUN ulimit -n 50000

EXPOSE 80

COPY <<EOF ./start-rinha.sh
#!/bin/bash
./scripts/env_replacement.sh && ./rinha2025-haskell
EOF
RUN chmod +x ./start-rinha.sh
RUN chown www-data /rinha2025-haskell

USER www-data:www-data
ENTRYPOINT ["./start-rinha.sh"]
