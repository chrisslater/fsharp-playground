module Api

type TrackResult<'Success, 'Message> =
    | Success of 'Success
    | Failure of 'Message

let succeed x =
    Success x

let fail msg =
    Failure msg

/// given a function wrapped in a result
/// and a value wrapped in a result
/// apply the function to the value only if both are Success
let applyR f result =
    match f,result with
    | Success (f), Success (x) -> 
        (f x) |> Success 
    | Failure err, Success (_) 
    | Success (_), Failure err -> 
        err |> Failure
    | Failure err1, Failure _ -> 
        err1  |> Failure 

/// given a function that transforms a value
/// apply it only if the result is on the Success branch
let liftR f result =
    let f' =  f |> succeed
    applyR f' result 

/// given two values wrapped in results apply a function to both
let lift2R f result1 result2 =
    let f' = liftR f result1
    applyR f' result2 

/// given three values wrapped in results apply a function to all
let lift3R f result1 result2 result3 =
    let f' = lift2R f result1 result2 
    applyR f' result3

/// given four values wrapped in results apply a function to all
let lift4R f result1 result2 result3 result4 =
    let f' = lift3R f result1 result2 result3 
    applyR f' result4

/// A function that applies either fSuccess or fFailure 
/// depending on the case.
let either fSuccess fFailure = function
    | Success x -> fSuccess x
    | Failure error -> fFailure error


/// given a function that generates a new RopResult
/// apply it only if the result is on the Success branch
/// merge any existing messages with the new result
let bindR f result =
    let fSuccess x = 
        f x
    let fFailure err = 
        Failure err
 
    either fSuccess fFailure result

/// given an RopResult, map the messages to a different error type
let mapMessageR f result = 
    match result with 
    | Success (x) -> 
        Success (x)
    | Failure error -> 
        let error = f error
        Failure error 

type OrderLine = {
    Id: string
    quantity: int
}

[<AllowNullLiteralAttribute>]
type ProductDto() = 
    member val Id : string = null with get, set
    member val Title : string = null with get, set
    member val Description : string = null with get, set

[<AllowNullLiteralAttribute>]
type PriceDto() = 
    member val Id : string = null with get, set
    member val ProductId : string = null with get, set
    member val Amount : float = 0.00 with get, set

type StringError =
    | Missing
    | MustNotBeLongerThan of int

module String10 =
    type T = String10 of string

    let create (s:string) =
        match s with
        | null -> fail StringError.Missing
        | _ when s.Length > 10 -> fail (StringError.MustNotBeLongerThan 10)
        | _ -> succeed (String10 s)

    let apply f (String10 s) =
        f s

module String30 =
    type T = String30 of string

    let create (s:string) =
        match s with
        | null -> fail StringError.Missing
        | _ when s.Length > 30 -> fail (StringError.MustNotBeLongerThan 30)
        | _ -> succeed (String30 s)

    let apply f (String30 s) =
        f s

module String200 =
    type T = String200 of string

    let create (s:string) =
        match s with
        | null -> fail StringError.Missing
        | _ when s.Length > 200 -> fail (StringError.MustNotBeLongerThan 200)
        | _ -> succeed (String200 s)

    let apply f (String200 s) =
        f s

type AmountError =
    | MustBeBiggerThanZero
    | MustNotBeBiggerThen of float

module Amount =
    type T = Amount of float

    let create (f: float) =
        match f with
        | _ when f < 0.01 -> fail AmountError.MustBeBiggerThanZero
        | _ when f > 999.99 -> fail (AmountError.MustNotBeBiggerThen 999.99)
        | _ -> succeed (Amount f)
    
    let apply f (Amount amount) =
        f amount
       

type Product = {
    Id: String10.T
    Title: String30.T
    Description: String200.T
}

type Price = {
    Id: String10.T
    ProductId: String10.T
    Amount: Amount.T
}

// let productsAsDto: List<ProductDto> = 
//     [
//         {
//             Id = "0001"
//             // Title = "Coat"
//             // Description = "Not really leather, that is cruel"
//         }
//         {
//             Id = "0002"
//             // Title = "Jumper"
//             // Description = "Happy Jumper"
//         }
//     ]


type DomainMessage =
    | ProductIdIsRequired
    | ProductIdMustNotBeLongerThan10
    | TitleIsRequired
    | TitleMustNotBeLongerThan30
    | DescriptionIsRequired
    | DescriptionMustNotBeLongerThan200

    | AmountMustBeBiggerThanZero
    | AmountIsTooBig
    
    | ProductIsRequired
    | PriceIsRequired

let createId id = 
    let map = function
        | StringError.Missing -> ProductIdIsRequired
        | StringError.MustNotBeLongerThan _ -> ProductIdMustNotBeLongerThan10

    // create the EmailAddress and convert the mes sages into the ones
    // appropriate for the domain
    String10.create id
    |> mapMessageR map

let createProductId = createId
let createPriceId = createId

let createTitle title =
    let map = function
        | StringError.Missing -> ProductIdIsRequired
        | StringError.MustNotBeLongerThan _ -> TitleMustNotBeLongerThan30

    // create the EmailAddress and convert the mes sages into the ones
    // appropriate for the domain
    String30.create title
    |> mapMessageR map

let createDescription description =
    let map = function
        | StringError.Missing -> ProductIdIsRequired
        | StringError.MustNotBeLongerThan _ -> DescriptionMustNotBeLongerThan200

    // create the EmailAddress and convert the mes sages into the ones
    // appropriate for the domain
    String200.create description
    |> mapMessageR map

let createAmount (amount: float) =
    let map = function
        | AmountError.MustBeBiggerThanZero -> DomainMessage.AmountMustBeBiggerThanZero
        | AmountError.MustNotBeBiggerThen _ -> DomainMessage.AmountIsTooBig

    // create the EmailAddress and convert the mes sages into the ones
    // appropriate for the domain
    Amount.create amount
    |> mapMessageR map


let createProduct id title description =
    {
        Id = id
        Title = title
        Description = description
    }

let dtoToProduct (dto: ProductDto) =
    if isNull dto then 
        fail DomainMessage.ProductIsRequired
    else
        let idOrError = createProductId dto.Id
        let titleOrError = createTitle dto.Title
        let descriptionOrError = createDescription dto.Description

        let productOrError = lift3R createProduct idOrError titleOrError descriptionOrError

        productOrError


let dtoToPrice (dto: PriceDto) =
    if isNull dto then 
        fail DomainMessage.PriceIsRequired
    else    
        let idOrError = createPriceId dto.Id
        let productIdOrError = createProductId dto.ProductId
        let amountOrError = createAmount dto.Amount

        let priceOrError = liftR createPrice idOrError productIdOrError amountOrError

        priceOrError


let findProductById (listOfProductDtos: List<ProductDto>) (id: string) =
    listOfProductDtos
    |> List.find(fun p -> p.Id = id)
    |> dtoToProduct


let convertDtosToProducts dtos =
    dtos
    |> List.map dtoToProduct



let findPriceByProductId (prices: List<PriceDto>)(id: string) =
    prices
    |> List.find(fun p -> p.Id = id)
    |> dtoToPrice