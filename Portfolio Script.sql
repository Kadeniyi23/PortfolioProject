--Looking at deaths
select *
from [Portfolio project].dbo.[Covid Deaths]
order by location,date

--Looking at the vaccinations
select *
from [Portfolio project].dbo.[Covid Vaccinations]
order by location,date

select location,date, population, total_cases,new_cases,total_deaths
from [Portfolio project].dbo.[Covid Deaths]
order by 1,2


--largest cases per country
select location, population,max(total_deaths/total_cases)*100 as DeathPercentage,MAX(total_cases/population) as percentofpoplation
from [Portfolio project].dbo.[Covid Deaths]
group by location,population
order by percentofpoplation desc

-- largest deaths per continent
select continent,max(cast(total_deaths as int)) as TotalDeathCount
from [Portfolio project].dbo.[Covid Deaths]
where continent is not null
group by continent
order by TotalDeathCount desc

--perecnt of people dead per infection
select date,max(total_cases)as maximum_case,max(total_deaths) as max_death,max((total_deaths/total_cases)*100) as DeathPercentage
from [Portfolio project].dbo.[Covid Deaths]
where continent is not null
group by date
order by 1,2 desc


--Perecent of world population infected or dead due to Covid
select date,population, sum(new_cases) as totalcases, sum(cast(new_deaths as int)) as totaldeaths, sum(cast(new_deaths as int))/sum(new_cases) as DeathPercentage, max(new_cases/population) as percentofpopulation
from [Portfolio project].dbo.[Covid Deaths]
where location='World'  and  new_cases != 0 and new_deaths !=0
group by date,population
order by date


with RollinPercentage (continent,location,date,population,new_vaccination,ROP)
as
(
select dea.continent,dea.location,dea.date, dea.population, vac.new_vaccinations,
SUM(convert(int,vac.new_vaccinations)) over (Partition by dea.Location order by dea.location,dea.date) as ROP
from [Portfolio project].dbo.[Covid Deaths] dea
join [Portfolio project].dbo.[Covid Vaccinations] vac
on dea.date=vac.date
and dea.location=vac.location
where dea.continent is not null
)

select *, ROP/population as percentageofpeoplevacc
from RollinPercentage

--Creating  a temp table to show the percent of people vaccinated
drop table if exists PercentPeopleVaccinated
create table PercentPeopleVaccinated
(continent nvarchar(255),
location nvarchar(255),
date datetime,
population numeric,
new_vaccinations numeric,
ROP numeric
)

Insert into PercentPeopleVaccinated
select dea.continent,dea.location,dea.date, dea.population, cast(vac.new_vaccinations as int),
SUM(convert(int,vac.new_vaccinations)) over (Partition by dea.Location order by dea.location,dea.date) as ROP
from [Portfolio project].dbo.[Covid Deaths] dea
join [Portfolio project].dbo.[Covid Vaccinations] vac
on dea.date=vac.date
and dea.location=vac.location
--where dea.continent is not null

select *, ROP/population
from PercentPeopleVaccinated
