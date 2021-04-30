{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bank where

import Generic.Random
import Test.QuickCheck
import qualified Test.QuickCheck as QC
import Data.Int
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace
import GHC.Generics (Generic)
import Data.List (delete)

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
  | RemoveCustomer CustomerId deriving (Show, Eq, Generic)

instance Arbitrary BankOperation where
  arbitrary = genericArbitrary uniform
  shrink (Withdraw cId money) = Withdraw cId <$> shrink money
  shrink (Deposit cId money) = Deposit cId <$> shrink money
  shrink (Transfer fromCId toCId money) = Transfer fromCId toCId <$> shrink money
  shrink (AddCustomer money) = AddCustomer <$> shrink money
  shrink (RemoveCustomer cId) = [RemoveCustomer cId]

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

nonEmptyBank :: Gen Bank
nonEmptyBank = do
  size <- getSize
  n <- choose (0, size)
  loop emptyBank (n + 1)
  where
  emptyBank = Map.empty
  loop :: Bank -> Int -> Gen Bank
  loop bank 0 = return bank
  loop bank i = arbitrary >>= (\amt -> loop (operate (AddCustomer amt) bank) (i-1))

bankWithMoreThan1Customer :: Gen Bank
bankWithMoreThan1Customer = do
  money <- arbitrary
  operate (AddCustomer money) <$> nonEmptyBank

transferDoesntChangeTheBalance :: Property
transferDoesntChangeTheBalance =
  forAll arbitraryBankTransfer $
    (\(oldBalance, newBalance, oldBank, fromCustId, toCustId, amtToTransfer) -> oldBalance == newBalance)
  where
  arbitraryBankTransfer = do
    bank <- nonEmptyBank
    let
      previousBalance = totalBalance bank
    fromCustId <- QC.elements (Map.keys bank)
    toCustId <- QC.elements (Map.keys bank)
    amtToTransfer <- arbitrarySizedBoundedIntegral
    let
      newBank = operate (Transfer fromCustId toCustId amtToTransfer) bank
      newBalance = totalBalance newBank
    return (previousBalance, newBalance, bank, fromCustId, toCustId, amtToTransfer)

depositIncreasesBankBalance :: Property
depositIncreasesBankBalance =
  forAll arbitraryBankDeposit $
    (\(oldBalance, newBalance, oldBank, custId, amtDeposited) -> oldBalance <= newBalance)
  where
  arbitraryBankDeposit = do
    bank <- nonEmptyBank
    let
      previousBalance = totalBalance bank
    custId <- QC.elements (Map.keys bank)
    amtToDeposit <- arbitrarySizedBoundedIntegral
    let
      newBank = operate (Deposit custId amtToDeposit) bank
      newBalance = totalBalance newBank
    return (previousBalance, newBalance, bank, custId, amtToDeposit)

alwaysHaveMoney :: Property
alwaysHaveMoney =
  forAll arbitraryBankOperations $
    (\(bank, operations, balance) -> balance >= 0)
  where
  arbitraryBankOperations = do
    bank <- nonEmptyBank
    operations <- listOf arbitrary
    let
      bankAfterOps = foldl (\b op -> operate op b) bank operations
    return (bank, operations, totalBalance bankAfterOps)

operationsNotInvolvingCustomer :: Bank -> CustomerId -> Gen BankOperation
operationsNotInvolvingCustomer bank cId =
  let
    allCustomersOtherThan = delete cId (customers bank)
  in
  oneof $
    [ Withdraw <$> elements allCustomersOtherThan <*> arbitrary
    , Deposit <$> elements allCustomersOtherThan <*> arbitrary
    , Transfer <$> elements allCustomersOtherThan <*> elements allCustomersOtherThan <*> arbitrary
    , AddCustomer <$> arbitrary
    , RemoveCustomer<$> elements allCustomersOtherThan
    ]

operationsNotInvolvingCustomerDoNotChangeHisBalance :: Property
operationsNotInvolvingCustomerDoNotChangeHisBalance =
  forAll bankCustomerAndOperationsNotInvolvingCustomer
    (\(bank, custId, operationsNotInvolvingCustomer) ->
      let
        custBalance = Map.lookup custId bank
        custBalanceAfterOperationsNotInvolvingCustomer =
          Map.lookup custId $ foldl (\b op -> operate op b) bank operationsNotInvolvingCustomer
      in custBalance == custBalanceAfterOperationsNotInvolvingCustomer)
  where
  bankCustomerAndOperationsNotInvolvingCustomer = do
    bank <- bankWithMoreThan1Customer
    custId <- QC.elements (Map.keys bank)
    operations <- listOf (operationsNotInvolvingCustomer bank custId)
    return (bank, custId, operations)
