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
RUN cabal configure \
#  -O0 \
   --extra-lib-dirs=/usr/lib/x86_64-linux-gnu \
   --enable-shared
#  --enable-executable-dynamic
#  --enable-shared  --enable-executable-dynamic

RUN cabal update && cabal install rinha2025-haskell \
  --installdir=/bin
COPY .env.prd /bin/
RUN mv /bin/.env.prd /bin/.env

FROM base
# COPY --from=build /servidor ./
COPY --from=build /bin ./
COPY --from=build /bin/.env ./bin/.env

EXPOSE 80

USER www-data:www-data
ENTRYPOINT ["rinha2025-haskell --verbose"]
