/*to view the data*/
select *
from product_sales

/*to count the number of rows in the table*/
select count(*)
from product_sales

/*For each column in the dataset, I validate and clean the column against the instructions given */

/*For the week column*/
SELECT COUNT(DISTINCT week) AS weeks
FROM product_sales


/*for the sales method column*/
SELECT COUNT(DISTINCT sales_method)
FROM product_sales

select sales_method,count(*)
from product_sales
group by sales_method

update product_sales
set sales_method = 'Email'
where sales_method = 'email'

update product_sales
set sales_method = 'Email + Call'
where sales_method = 'em + call'

/*for the cutomer id column*/
SELECT COUNT(DISTINCT customer_id)
FROM product_sales

/*checking for duplicates*/
SELECT customer_id, COUNT(*) as count
FROM product_sales
GROUP BY customer_id
HAVING count > 1;


/*nb_sold column*/
SELECT COUNT(DISTINCT nb_sold)
FROM product_sales

/*revenue column*/
SELECT sales_method,count(*),avg(revenue),sum(revenue)
FROM product_sales
group by sales_method

/*to find the mean and median values of revenue by sales method*/
SELECT sales_method,count(*),avg(revenue)
from product_sales
where revenue =  0
group by sales_method


/*to change the missing values to the mean of the respective sales method*/
UPDATE product_sales
SET revenue = (
  SELECT AVG(revenue) 
  FROM product_sales 
  WHERE sales_method = 'Call' 
    AND revenue = 'NA'
)
WHERE revenue = 'NA'
  AND sales_method = 'Call';
  

UPDATE product_sales
SET revenue = (
  SELECT AVG(revenue) 
  FROM product_sales 
  WHERE sales_method = 'Email' 
    AND revenue = 'NA'
)
WHERE revenue = 'NA'
  AND sales_method = 'Email';
  
UPDATE product_sales
SET revenue = (
  SELECT AVG(revenue) 
  FROM product_sales 
  WHERE sales_method = 'Email + Call' 
    AND revenue = 'NA'
)
WHERE revenue ='NA'
  AND sales_method = 'Email + Call';
  
/*to change the values to 2 decimal places*/
UPDATE product_sales
SET revenue = ROUND(revenue, 2)
WHERE revenue IS NOT NULL;

/*for the years as customer column*/
SELECT sales_method,count(*),avg(revenue),avg(years_as_customer)
FROM product_sales
group by sales_method

update product_sales
set years_as_customer = 0
where years_as_customer > 39
  

/*for the nb site visits column*/
SELECT COUNT(DISTINCT nb_site_visits)
FROM product_sales

/*for the state column*/
SELECT COUNT(DISTINCT state)
FROM product_sales


/*to check for missing values in all the columns*/
SELECT *
FROM product_sales
WHERE NULL IN (week,sales_method,customer_id,nb_sold,revenue,years_as_customer,nb_site_visits,state)

/*Exploratory data analysis*/
/*to find the number of customers for each approach*/
SELECT sales_method,count(*)
FROM product_sales
group by sales_method
