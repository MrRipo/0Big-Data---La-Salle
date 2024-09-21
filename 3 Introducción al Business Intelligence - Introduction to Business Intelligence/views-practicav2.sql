-- ==================================================================================
-- 1. Modelo Dimensional
-- ==================================================================================

-- ----------------------------------------------------------------------------------
-- Dimensión DT_PRODUCT
-- ----------------------------------------------------------------------------------
Create Or Replace View introbibigqueryp01.dataset_sales.DT_PRODUCT As
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

-- ----------------------------------------------------------------------------------
-- Dimensión DT_TIME
-- ----------------------------------------------------------------------------------
Create Or Replace View introbibigqueryp01.dataset_sales.DT_TIME AS
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

-- ----------------------------------------------------------------------------------
-- Dimensión DT_TICKET_LINE
-- ----------------------------------------------------------------------------------
Create Or Replace View introbibigqueryp01.dataset_sales.DT_TICKET_LINE As
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

-- ----------------------------------------------------------------------------------
-- Dimensión DT_CUSTOMER
-- ----------------------------------------------------------------------------------
Create Or Replace View introbibigqueryp01.dataset_sales.DT_CUSTOMER As
With customers_ids As (
    Select email, max(id) as MaxCustId, Min(id) As MinCustId
    From `bigquery-public-data.thelook_ecommerce.users`
    Group By email

), customers_info_max As (
    Select
        cust.email
    ,   cust.first_name
    ,   cust.last_name
    ,   concat(trim(cust.first_name),', ',trim(cust.last_name)) As full_name
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
    From `bigquery-public-data.thelook_ecommerce.users` cust
    Join customers_ids custIds
        On (cust.id=custIds.MaxCustId)
    Where cust.email=custIds.email

), customers_info_min AS (
    Select
        cust.email
    ,   cust.created_at
    ,   ROUND(CEILING(TIMESTAMP_DIFF(CURRENT_TIMESTAMP(), cust.created_at, DAY)/365),0) As creation_lifespan_years
    From `bigquery-public-data.thelook_ecommerce.users` cust
    Join customers_ids custIds
        On (cust.id=custIds.MinCustId)
    Where cust.email=custIds.email
)
Select max.*, min.* EXCEPT(email)
From customers_info_max max
Join customers_info_min min
    On (max.email=min.email)
;

-- ----------------------------------------------------------------------------------
-- Tabla de Hechos FT_SALES
-- ----------------------------------------------------------------------------------
Create Or Replace View introbibigqueryp01.dataset_sales.FT_SALES As
With orders_ext As (
    Select users.email
    ,   orders.order_id     As ticket_id
    ,   orders.created_at   As time_id
    ,   ROUND(CEIL(TIMESTAMP_DIFF(orders.delivered_at, orders.created_at,DAY)),0) As days_from_creation_to_delivery
    ,   ROUND(CEIL(TIMESTAMP_DIFF(orders.shipped_at, orders.created_at,DAY)),0) As days_from_creation_to_shipment
    ,   ROUND(CEIL(TIMESTAMP_DIFF(orders.delivered_at, orders.shipped_at,DAY)),0) As days_from_shipment_to_delivery
    From `bigquery-public-data.thelook_ecommerce.orders` orders
    Join `bigquery-public-data.thelook_ecommerce.users` users
        on (orders.user_id=users.id)
), items_ext As (
    Select items.order_id       As ticket_id
    ,   items.id                As ticket_line_id
    ,   products.id             As product_id
    ,   1                       As total_products
    ,   Case When items.status in ('Complete','Shipped','Processing')
            Then items.sale_price
            Else 0
        End                     As net_sales
    ,   products.retail_price   As gross_sales
    ,   (products.retail_price - items.sale_price) As total_discount
    From `bigquery-public-data.thelook_ecommerce.order_items` items
    Join `bigquery-public-data.thelook_ecommerce.products` products
        On (items.product_id=products.id)
)
Select
    items.ticket_line_id
,   orders.email
,   items.product_id
,   orders.time_id
,   orders.days_from_creation_to_delivery
,   orders.days_from_creation_to_shipment
,   orders.days_from_shipment_to_delivery
,   items.total_products
,   items.net_sales
,   items.gross_sales
,   items.total_discount
From orders_ext orders Left Join items_ext items
    On (orders.ticket_id = items.ticket_id)
;

-- ==================================================================================
-- 2. Vista Capa Semántica
-- ==================================================================================

Create Or Replace View introbibigqueryp01.dataset_sales.V_SALES_SEMANTIC_LAYER As
Select FT_SALES.email, * EXCEPT
    (   ticket_id
    ,   ticket_line_id
    ,   product_id
    ,   time_id
    ,   email
    )
From introbibigqueryp01.dataset_sales.FT_SALES
Join introbibigqueryp01.dataset_sales.DT_TICKET_LINE
    On (FT_SALES.ticket_line_id=DT_TICKET_LINE.ticket_line_id)
Left Join introbibigqueryp01.dataset_sales.DT_PRODUCT
    On (FT_SALES.product_id=DT_PRODUCT.product_id)
Left Join introbibigqueryp01.dataset_sales.DT_TIME
    On (FT_SALES.time_id=DT_TIME.time_id)
Left Join introbibigqueryp01.dataset_sales.DT_CUSTOMER
    On (FT_SALES.email=DT_CUSTOMER.email)
;

-- ==================================================================================
-- 3. Preguntas
-- ==================================================================================
-- a) ¿Cuál es el producto más vendido (gross_sales)? 
--      ¿A qué marca (brand) pertenece? 
--      ¿A qué categoría? 
--      ¿Cuántos ingresos (net sales) ha proporcionado? 
--      Asumiremos todos los importes están en EUR.

-- Si la pregunta realmente es: el producto más vendido (max productos_vendidos):
Select vsl.name
     , vsl.category
     , vsl.brand
     , Sum(vsl.total_products) As productos_vendidos
     , Sum(vsl.gross_sales) as sum_ventas_brutas
     , Sum(vsl.net_sales)   as sum_ventas_netas
From introbibigqueryp01.dataset_sales.V_SALES_SEMANTIC_LAYER vsl
Where vsl.order_status in ('Complete', 'Shipped', 'Processing') -- Duda
Group By vsl.name, vsl.category, vsl.brand
Order By Sum(vsl.total_products) Desc
Limit 1
;

-- Si la pregunta realmente es: el producto que más ventas brutas generó (max sum_ventas_brutas):
Select vsl.name
     , vsl.category
     , vsl.brand
     , Sum(vsl.total_products) As productos_vendidos
     , Sum(vsl.gross_sales) as sum_ventas_brutas
     , Sum(vsl.net_sales)   as sum_ventas_netas
From introbibigqueryp01.dataset_sales.V_SALES_SEMANTIC_LAYER vsl
Where vsl.order_status in ('Complete', 'Shipped', 'Processing') -- Duda
Group By vsl.name, vsl.category, vsl.brand
Order By Sum(vsl.gross_sales) Desc
Limit 1
;


-- b) ¿Quién es el mejor cliente? 
--      ¿Cuántos ingresos ha generado? 
--      Utilizar indicador net_sales
Select vsl.email
     , vsl.full_name
     , sum(vsl.net_sales) as sum_net_sales
From introbibigqueryp01.dataset_sales.V_SALES_SEMANTIC_LAYER vsl
Group By vsl.email, vsl.full_name
Order By sum(vsl.net_sales) Desc
Limit 1
;


-- c) ¿Qué año ha sido el que “theLook” ha ingresado más? 
--      Utilizar indicador net_sales
Select vsl.year, SUM(vsl.net_sales) As sum_net_sales
From introbibigqueryp01.dataset_sales.V_SALES_SEMANTIC_LAYER vsl
Group By vsl.year
Order By SUM(vsl.net_sales) Desc
Limit 1
;

-- d) ¿Cuántos clientes únicos tiene “theLook”?
Select count(distinct email) as clientes_unicos
From introbibigqueryp01.dataset_sales.V_SALES_SEMANTIC_LAYER
;

-- e) ¿Cuál es el promedio en días entre: 
--      creación y envío, 
--      creación y entrega, 
--      y entre envío y entrega de los pedidos? 
--      
--      En caso “theLook” quiera optimizar el tiempo total del pedido, 
--      ¿en qué proceso de la cadena de envío debería invertir recursos para mejorar?

Select avg(vsl.days_from_creation_to_shipment) as prom_ceacion_envio
    ,  avg(vsl.days_from_creation_to_delivery) as prom_creacion_entrega
    ,  avg(vsl.days_from_shipment_to_delivery) as prom_envio_entrega
From introbibigqueryp01.dataset_sales.V_SALES_SEMANTIC_LAYER vsl
;
