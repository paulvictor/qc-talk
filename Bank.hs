{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bank where

import Generic.Random
import Data.Int
import Data.Word
import Data.Map (Map)
import Data.Map ((!))
import qualified Data.Map as Map
import Debug.Trace
import Data.List (delete)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type CustomerId = Int
type Money = Word
type Bank = Map CustomerId Money

totalBalance :: Bank -> Word
totalBalance = sum . Map.elems

customers :: Bank -> [ CustomerId ]
customers = Map.keys

data BankOperation
  = Withdraw CustomerId Money
  | Deposit CustomerId Money
  | Transfer CustomerId CustomerId Money
  | AddCustomer Money
  | RemoveCustomer CustomerId deriving (Show, Eq)

operate :: BankOperation -> Bank -> Bank
operate (Withdraw custId amt) bank =
  Map.alter (fmap (\bal -> if bal >= amt then bal-amt else bal)) custId bank
operate (Deposit custId amt) bank = Map.alter (fmap (\bal -> bal+amt)) custId bank
operate (Transfer fromCustId toCustId amt) bank =
  let
    hasBalance = maybe False (>amt) $ Map.lookup fromCustId bank
    withDrawn = Map.alter (fmap (\bal -> bal-amt)) fromCustId bank
  in if hasBalance then Map.alter (fmap (\bal -> bal+amt)) toCustId withDrawn else bank
operate (AddCustomer newAmt) bank =
  Map.insert (Map.size bank + 1) newAmt bank
operate (RemoveCustomer custId) bank =
  Map.delete custId bank

randomMoney = Gen.word (Range.linear 1 100000)

nonEmptyBank :: Gen Bank
nonEmptyBank = do
  size <- Gen.int (Range.linear 1 100)
  loop emptyBank size
  where
  emptyBank = Map.empty
  loop :: Bank -> Int -> Gen Bank
  loop bank 0 = return bank
  loop bank i =  randomMoney >>= (\amt -> loop (operate (AddCustomer amt) bank) (i-1))

bankWithMoreThan1Customer :: Gen Bank
bankWithMoreThan1Customer = do
  money <- randomMoney
  operate (AddCustomer money) <$> nonEmptyBank

depositIncreasesBankBalance :: Property
depositIncreasesBankBalance =
  property $ do
    bank <- forAll nonEmptyBank
    cust <- forAll $ Gen.element (customers bank)
    amtToDeposit <- forAll $ Gen.word (Range.linear 1 5000000)
    let
      newBank = operate (Deposit cust amtToDeposit) bank
    assert $ bank ! cust < newBank ! cust

transferDoesntChangeTheBalance :: Property
transferDoesntChangeTheBalance =
  property $ do
    bank <- forAll nonEmptyBank
    fromCust <- forAll $ Gen.element (customers bank)
    toCust <- forAll $ Gen.element (customers bank)
    amtToTransfer <- forAll $ Gen.word (Range.linear 0 (bank ! fromCust))
    let
      newBank = operate (Transfer fromCust toCust amtToTransfer) bank
    totalBalance bank === totalBalance newBank

transferReducesTheBalance :: Property
transferReducesTheBalance =
  property $ do
    bank <- forAll nonEmptyBank
    fromCust <- forAll $ Gen.element (customers bank)
    toCust <- forAll $ Gen.element (customers bank)
    amtToTransfer <- forAll $ Gen.word (Range.linear 0 (bank ! fromCust))
    let
      newBank = operate (Transfer fromCust toCust amtToTransfer) bank
    assert $ newBank ! fromCust < bank ! fromCust

genOperations :: Bank -> Gen BankOperation
genOperations bank =
  Gen.choice
    [ genWithdrawal
    , genDeposit
    , genTransfer
    , genCreateCustomer
    , genRemoveCustomer ]
  where
  cust = Gen.element (customers bank)
  amt = Gen.word (Range.linearBounded)
  genWithdrawal = Withdraw <$> cust <*> amt
  genDeposit = Deposit <$> cust <*> amt
  genTransfer = Transfer <$> cust <*> cust <*> amt
  genCreateCustomer = AddCustomer <$> amt
  genRemoveCustomer = RemoveCustomer <$> cust

operationsNotInvolvingCustomer :: CustomerId -> Bank -> Gen BankOperation
operationsNotInvolvingCustomer custId bank =
  genOperations (Map.delete custId bank)

operationsNotInvolvingCustomerDoNotChangeHisBalance :: Property
operationsNotInvolvingCustomerDoNotChangeHisBalance =
  property $ do
    bank <- forAll bankWithMoreThan1Customer
    cust <- forAll $ Gen.element (customers bank)
    operations <- forAll $ Gen.list (Range.linear 0 10000) (operationsNotInvolvingCustomer cust bank)
    let
      custBalancePreOperations = Map.lookup cust bank
      custBalanceAfterOperations =
        Map.lookup cust $ foldl (\b op -> operate op b) bank operations
    custBalancePreOperations === custBalanceAfterOperations

--
--alwaysHaveMoney :: Property
--alwaysHaveMoney =
--  forAll arbitraryBankOperations $
--    (\(bank, operations, balance) -> balance >= 0)
--  where
--  arbitraryBankOperations = do
--    bank <- nonEmptyBank
--    operations <- listOf arbitrary
--    let
--      bankAfterOps = foldl (\b op -> operate op b) bank operations
--    return (bank, operations, totalBalance bankAfterOps)
--
