Create View introbibigqueryp01.dataset_sales.DT_PRODUCT As
    Select
        products.id as product_id
    ,   Case 
            When (products.name Is Null Or Trim(products.name)='') 
            Then '<NO-NAME>' 
            Else Trim(products.name) 
        End as name
    ,   Case 
            When (products.category Is Null Or Trim(products.category)='') 
            Then '<NO-CATEGORY>' 
            Else Trim(products.category) 
        End as category,
        Case
            When (products.brand Is Null Or Trim(products.brand)='')
            Then '<NO-BRAND>'
            Else Trim(products.brand)
        End as brand
    From 
        `bigquery-public-data.thelook_ecommerce.products` products
    ;

Create View introbibigqueryp01.dataset_sales.DT_TIME AS
    Select Distinct
        orders.created_at                                                   As time_id
    ,   Cast(Extract(YEAR From orders.created_at) As String)                As year
    ,   LPad(Cast(Extract(MONTH From orders.created_at) As String), 2, '0') As month
    ,   LPad(Cast(Extract(DAY From orders.created_at) As String), 2, '0')   As day
    ,   FORMAT_TIMESTAMP('%Y_%m', orders.created_at)                        As yyyy_mm
    ,   FORMAT_TIMESTAMP('%Y_%m_%d', orders.created_at)                     As yyyy_mm_dd
    from 
        `bigquery-public-data.thelook_ecommerce.orders` orders
    ;

Create View introbibigqueryp01.dataset_sales.DT_TICKET_LINE As
    Select
        items.id            As ticket_line_id
    ,   items.order_id      As ticket_id
    ,   orders.status       As order_status
    ,   items.status        As order_line_status
    ,   orders.created_at   As order_created_at
    From
        `bigquery-public-data.thelook_ecommerce.order_items` items
    Inner Join
        `bigquery-public-data.thelook_ecommerce.orders` orders
        on (orders.order_id=items.order_id)
    ;    

Create View introbibigqueryp01.dataset_sales.DT_CUSTOMER As
    With customers As (
        Select
            cust.email
        ,   cust.first_name
        ,   cust.last_name
        ,   concat(trim(cust.first_name), ', ', trim(cust.last_name)) As full_name
        ,   cust.age
        ,   Case
                When (cust.age < 18) Then '(0,18)'
                When (cust.age >=18 And cust.age<25) Then '[18,25)'
                When (cust.age >=25 And cust.age<35) Then '[25,35)'
                When (cust.age >=35 And cust.age<45) Then '[35,45)'
                When (cust.age >=45 And cust.age<60) Then '[45,60)'
                When (cust.age >=60) Then '[60,-)'
                Else '<N/A>'
            End As age_range
        ,   cust.gender
        ,   cust.country
        ,   cust.city
        ,   cust.traffic_source
        ,   cust.created_at
        ,   ROUND(CEILING(TIMESTAMP_DIFF(CURRENT_TIMESTAMP(), cust.created_at, DAY)/355),0) As creation_lifespan_years
        From `bigquery-public-data.thelook_ecommerce.users` cust
    )
    Select
        email
    ,   MAX(first_name) As first_name
    ,   MAX(last_name)  As last_name
    ,   MAX(full_name)  As full_name
    ,   MAX(age)        As age
    ,   MAX(age_range)  As age_range
    ,   MAX(gender)     As gender
    ,   MAX(country)    As country
    ,   MAX(city)       As city
    ,   MAX(traffic_source) As traffic_source
    ,   MIN(created_at) As created_at
    ,   MAX(creation_lifespan_years) As creation_lifespan_years
    From customers
    Group By email
;

