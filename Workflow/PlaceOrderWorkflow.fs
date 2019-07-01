module PlaceOrderWorkflow

open DomainApi

type ValidatedOrder = {
    OrderId: OrderId
    CustomerInfo: CustomerInfo
    // ShippingAddress: Address
    // BillingAddress: Address
}
and CustomerInfo = {
    Name: PersonalName
    Email: EmailAddress
}
and PersonalName = {
    FirstName: String50
    LastName: String50
}
and Address = {
    AddressLine1: String50
    AddressLine2: String50 option
    PostCode: PostCode
}

type Order =
    | Unvalidated of UnvalidatedOrder
    | Validated of ValidatedOrder


// ----------
// Code
// ----------

let toCustomerInfo (customer: UnvalidatedCustomerInfo): CustomerInfo =
    let firstName = customer.FirstName |> String50.create
    let lastName = customer.LastName |> String50.create
    let emailAddress = customer.Email |> EmailAddress.create

    let name: PersonalName = {
        FirstName = firstName
        LastName = lastName
    }

    let customerInfo: CustomerInfo = {
        Name = name
        Email = emailAddress
    }

    customerInfo

type CheckedAddress = {
    AddressLine1: string
    AddressLine2: string option
    PostCode: string
}

type CheckAddressExists = UnvalidatedAddress -> CheckedAddress

let checkAddressExists (unvalidatedAddress: UnvalidatedAddress): CheckedAddress =
    let checkedAddress: CheckedAddress = {
        AddressLine1 = unvalidatedAddress.AddressLine1
        AddressLine2 = unvalidatedAddress.AddressLine2
        PostCode = unvalidatedAddress.PostCode
    }

    checkedAddress

let toAddress (checkAddressExists: CheckAddressExists) unvalidatedAddress: Address =
    let checkedAddress = checkAddressExists unvalidatedAddress

    let addressLine1 = checkedAddress.AddressLine1 |> String50.create
    let addressLine2 = checkedAddress.AddressLine2 |> String50.createOption
    let postCode = checkedAddress.PostCode |> PostCode.create

    let address: Address = {
        AddressLine1 = addressLine1
        AddressLine2 = addressLine2
        PostCode = postCode
    }

    address


type ValidateOrder =
    UnvalidatedOrder
        -> ValidatedOrder

let validateOrder: ValidateOrder =
    fun unvalidatedOrder ->
        let orderId =
            unvalidatedOrder.OrderId
            |> OrderId.create

        let customerInfo =
            unvalidatedOrder.CustomerInfo
            |> toCustomerInfo

        let shippingAddress =
            unvalidatedOrder.ShippingAddress
            |> toAddress

        {
            OrderId = orderId
            CustomerInfo = customerInfo
            ShippingAddress = shippingAddress
        }
    

let placeOrder unvalidatedOrder =
    |> validateOrder