
/*To select all the rows where there is no data ie columns where there is null and remove them
*/
select *
from [Portfolio project].dbo.[linkedin-jobs-africa]
where title is null

delete from [Portfolio project].dbo.[linkedin-jobs-africa]
where title is null


/*Breaking out Location into Individual Columns (City, District and Country)*/
Select *,
PARSENAME(REPLACE(location, ',', '.') , 3)
,PARSENAME(REPLACE(location, ',', '.') , 2)
,PARSENAME(REPLACE(location, ',', '.') , 1)
from [Portfolio project].dbo.[linkedin-jobs-africa]



/* To add the city,district and Country name to the table*/
alter table [Portfolio project].dbo.[linkedin-jobs-africa]
Add City Nvarchar(255);

update [Portfolio project].dbo.[linkedin-jobs-africa]
set City = PARSENAME(REPLACE(location, ',', '.') , 3)

alter table [Portfolio project].dbo.[linkedin-jobs-africa]
Add District_or_Province Nvarchar(255);

update  [Portfolio project].dbo.[linkedin-jobs-africa]
set District_or_Province = PARSENAME(REPLACE(location, ',', '.') , 2)

alter table [Portfolio project].dbo.[linkedin-jobs-africa]
Add Country Nvarchar(255);

Update [Portfolio project].dbo.[linkedin-jobs-africa]
set Country = PARSENAME(REPLACE(location, ',', '.') , 1)

/*To view all the columns in the data again */
select *
from [Portfolio project].dbo.[linkedin-jobs-africa]
order by posted_date desc


/* to count the number of onsite,hybrid and remote jobs*/
select onsite_remote ,COUNT(onsite_remote)
from [Portfolio project].dbo.[linkedin-jobs-africa]
group by onsite_remote

/* to convert the company column to a uniform upper case*/
update[Portfolio project].dbo.[linkedin-jobs-africa]
set company = UPPER(company) 

/* Because many of the columns are empty in the city and district province I
set the rows where they are null to values in City*/

update [Portfolio project].dbo.[linkedin-jobs-africa]
set District_or_Province= case when District_or_Province is null then Country
else District_or_Province
end

update [Portfolio project].dbo.[linkedin-jobs-africa]
set City= case when City is null then District_or_Province
else City
end

/*To extract date from the posted date column*/
select cast(posted_date as date)
from [Portfolio project].dbo.[linkedin-jobs-africa]

update [Portfolio project].dbo.[linkedin-jobs-africa]
set posted_date = convert(date,posted_date) 

/* to select all columns with the title of junior analyst or graduate analyst*/
select *
from [Portfolio project].dbo.[linkedin-jobs-africa]
where title like '%Junior%' or title like '%Graduate%'

/*to select all titles from the company 'KUDA'*/
select company,title,posted_date
from [Portfolio project].dbo.[linkedin-jobs-africa]
where company = 'KUDA'

/*to select companies who required SQL Developer*/
select company,title,posted_date
from [Portfolio project].dbo.[linkedin-jobs-africa]
where title like '%SQL%'

/*To remove gibberish in the criteria column*/
update [Portfolio project].dbo.[linkedin-jobs-africa]
set criteria='Not Available' where criteria like '%Ø%'


/*To remove duplicate job titles, we create a CTE*/


WITH RowNumCTE AS(
Select *,
	ROW_NUMBER() OVER (
	PARTITION BY title,
				 company,
				 description,
				 posted_date,
				 City,
				 onsite_remote,
				 salary,
				 criteria,
				 link
				 ORDER BY
					City
					) row_num

From [Portfolio project].dbo.[linkedin-jobs-africa]
)
Select *
From RowNumCTE
Where row_num >1
Order by City


/*to remove columns that are not needed*/

alter table [Portfolio project].dbo.[linkedin-jobs-africa]
drop column location,Seniority_Level, seniority,criteria


select *
from [Portfolio project].dbo.criteria
