{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pagamento.ApiLib.ObterPagamentoPorId
  ( obterPagamentoPorId
  ) where

import Control.Monad.IO.Class
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as SQL
import Servant
import Pagamento.ViewModelsLib.PaymentSyncVM (PaymentSync)

obterPagamentoPorId :: DP.Pool SQL.Connection -> Int -> Handler PaymentSync
obterPagamentoPorId conns idPagamento = error "Not implemented"
{-
  transacoes <- fmap (map fromRowTransacao) . liftIO $
    withResource conns $ \conn ->
      (query conn
        (  "SELECT valor, tipo, descricao, realizada_em"
        <> " FROM Transacoes WHERE id_cliente = ?"
        <> " ORDER BY realizada_em DESC"
        <> " LIMIT 10;"
        )
        ( Only idCliente :: Only Int )
        :: IO [(Int, Text, Text, UTCTime)])

  saldos <- fmap (map fromRowSaldo) . liftIO $
    withResource conns $ \conn ->
      (query conn
        (  "SELECT saldo, limite, NOW()"
        <> " FROM Pagamento WHERE id = ?;"
        )
        ( Only idCliente :: Only Int )
        :: IO [(Int, Int, UTCTime)])

  case saldos of
    []         -> throwError err404
    (xsaldo:_) -> return ExtratoNaHora { saldo = xsaldo , ultimas_transacoes = transacoes }

-}
