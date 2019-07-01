module DomainApi

open System

// -------
// General
// -------

type AsyncResult<'success, 'failure> = Async<Result<'success, 'failure>>

// -------
// Input
// -------

type UnvalidatedOrder = {
    OrderId: string
    CustomerInfo: UnvalidatedCustomerInfo
    ShippingAddress: UnvalidatedAddress
    BillingAddress: UnvalidatedAddress
}
and UnvalidatedCustomerInfo = {
    FirstName: string
    LastName: string
    Email: string
}
and UnvalidatedAddress = {
    AddressLine1: string
    AddressLine2: string option
    PostCode: string
}

type Command<'data> = {
    Data: 'data
    TimeStamp: DateTime
    UserId: string
}

type PlaceOrderCommand = Command<UnvalidatedOrder>

// ----------
// Public API
// ----------

type OrderPlaced = {
    OrderId: string
}

type PlaceOrderEvent = 
    | OrderPlaced of OrderPlaced

type PlaceOrderFailure = {
    Code: string
    Message: string
}

type PlaceOrderWorkflow = 
    PlaceOrderCommand
        -> AsyncResult<PlaceOrderEvent, PlaceOrderFailure>

  


type OrderId = private OrderId of string

module OrderId =
    let create str =
        if String.IsNullOrEmpty(str) then
            failwith "OrderId must not be null or empty"
        elif str.Length > 50 then
            failwith "OrderId must not be more than 50 chars"
        else
            OrderId str
    let value (OrderId str) =
        str

type EmailAddress = private EmailAddress of string

module EmailAddress =
    let create str = 
        if String.IsNullOrEmpty(str) then
            failwith "EmailAddress must not be null or empty"
        elif str.Length > 50 then
            failwith "EmailAddress must not be more than 50 chars"
        else
            EmailAddress str
        
    let value  (EmailAddress str) = 
        str

type String50 = private String50 of string
    
module String50 =
    let create str =
        if String.IsNullOrEmpty(str) then
            failwith "String must not be null or empty"
        elif str.Length > 50 then
            failwith "String must not be more than 50 chars"
        else
            String50 str

    let createOption str =
        if String.IsNullOrEmpty(str) then
            None
        elif str.Length > 50 then
            failwith "String must not be more than 50 chars"
        else
            let optionalString50 = String50 str

            Some optionalString50
        
    let value (String50 str) = str

type PostCode = private PostCode of string

module PostCode =
    let create str =
        if String.IsNullOrEmpty(str) then
            failwith "PostCode must not be null or empty"
        elif str.Length > 7 then
            failwith "PostCode must not be more than 7 chars"
        else 
            PostCode str

    let value (PostCode str) = str
    

        