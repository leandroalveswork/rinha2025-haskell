# Revision history for rinha2025-haskell

## 0.8.0 -- 2025-07-15

* Primeira versão: APIs de pagar e sumarizar, retentativas, analise de quantias

## 0.8.1 -- 2025-07-21

* CI/CD completo

## 0.8.2 -- 2025-07-28

* Arquivos de automação do Nix de SUBIDA e PARADA do processor-container e Postgres

## 0.9.1 -- 2025-07-29

* Tornar configuravel de chamar qual hostname das APIs de integração
* Correção de reagendar retentativa corretamente após 15 segundos
* Correção no Get dos pagamentos

## 0.9.2 -- 2025-08-01

* Processamento em paralelo

## 1.0.0 -- 2025-08-07

* API de gateway: POST /payments
* API informativa: GET /payments-summary
* Entrypoint de /payments sempre devolve OK, processando em segundo plano
* Retentativas de 10 em 10 segundos
* Trabalho dividido na proporção 2:1 entre os dois servidores de back-end
* Segurança em apenas ter uma thread lidando com o mesmo pagamento
* Ambiente de QA separado de PRD
* Scripts nix para subida do container-processor, banco de dados e load balancer
* Bugfixes de queries e race condition
