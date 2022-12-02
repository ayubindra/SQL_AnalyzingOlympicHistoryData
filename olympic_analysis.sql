-- change table name of olympic_history_noc_regions to noc_regions
alter table olympic_history_noc_regions rename to noc_regions;

-- inspecting first 5 rows of olympic_history table
select * from olympic_history limit 5;

-- inspecting first 5 rows of noc_regions table
select * from noc_regions limit 5;

-- (1) How many olympic games have been held?
select count (distinct year) as number_of_olympic_held from olympic_history;

-- (2) List down all Olympics games held so far
select distinct year, season, city from olympic_history order by year desc;

-- (3) Mention the total no of nations who participated in each olympics game?
with all_nations as (
    select o.games, n.region
    from olympic_history as o
    join noc_regions as n
        on o.noc = n.noc
    group by o.games, n.region
)
select games, count(games) total_participated
from all_nations
group by games
order by games desc;

-- (4) Which year saw the highest and lowest no of countries participating in olympics?
with country_count as(
    select games, count(distinct noc) as total_countries
    from olympic_history
    group by games
    order by games
)
select distinct concat(first_value(games) over(order by total_countries), ' - ', 
                       first_value(total_countries) over (order by total_countries)) as lowest_countries,
                concat(first_value(games) over(order by total_countries desc), ' - ', 
                       first_value(total_countries) over (order by total_countries desc)) as highest_countries
from country_count
order by 1;

-- (5) Which nation has participated in all of the olympic games
with country_participation as
    (select n.region as country, count(distinct o.games) as num_participation
    from olympic_history as o
    join noc_regions as n
         on o.noc = n.noc
    group by n.region
    order by num_participation desc)
select country, num_participation
from country_participation
where num_participation = (select max(num_participation) from country_participation);

-- (6) Identify the sport which was played in all summer olympics.
with num_sport_held as
    (select sport, count(distinct games) as num_sport
    from olympic_history
    group by sport
    order by num_sport desc)
select sport, num_sport, (select max(num_sport) from num_sport_held) as sport_total
from num_sport_held
where num_sport = (select max(num_sport) from num_sport_held);

-- (7) Which Sports were just played only once in the olympics.
with t1 as
    (select distinct games, sport
     from olympic_history),
     t2 as
    (select sport, count(1) as no_of_games
     from t1
     group by sport
     order by sport)
select t2.*, t1.games 
from t2
join t1 on t1.sport = t2.sport
where t2.no_of_games = 1
order by t1.sport;

-- (8) Fetch the total no of sports played in each olympic games.
select games, count(distinct sport) no_of_sport
from olympic_history
group by games
order by games desc;

-- (9) Fetch oldest athletes to win a gold medal.
with temp as
    (select name, sex, cast(case when age = 'NA' then '0' else age end as int) as age, games, sport, medal
     from olympic_history),
     ranking as 
    (select *, rank() over (order by age desc) as rank
     from temp
     where medal = 'Gold')
select *
from ranking
where rank = 1;

-- (10) Find the Ratio of male and female athletes participated in all olympic games.
with t1 as
     (select sex, count(1) as cnt
      from olympic_history
      group by sex),
     t2 as
     (select *, row_number() over(order by cnt) as rn
      from t1),
     max_cnt as
     (select cnt from t2 where rn = 2),
     min_cnt as
     (select cnt from t2 where rn = 1)
select concat(1, ' : ', round(max_cnt.cnt::decimal/min_cnt.cnt, 2)) as sex_ratio
from min_cnt, max_cnt;

-- (11) Fetch the top 5 athletes who have won the most gold medals.
with gold_medal_rank as
    (select name, 
        noc, 
        count(medal) as num_of_gold_medal, 
        dense_rank() over(order by count(medal) desc) as ranking
    from olympic_history
    where medal = 'Gold'
    group by name, noc
    order by num_of_gold_medal desc)
select *
from gold_medal_rank
where ranking between 1 and 5;

-- (12) Fetch the top 5 athletes who have won the most medals (gold/silver/bronze).
with medal_rank as
    (select name,
        noc,
        count(medal) as num_of_medal,
        dense_rank() over (order by count(medal) desc) as ranking
        from olympic_history
        where medal != 'NA'
        group by name, noc
        order by num_of_medal desc)
select name, noc, num_of_medal 
from medal_rank
where ranking between 1 and 5;

-- (13) Fetch the top 5 most successful countries in olympics. Success is defined by no of medals won.
with medal_rank as
    (select n.region as country, 
        count(medal) as num_of_medal, 
        dense_rank() over(order by count(medal) desc) as ranking
    from olympic_history as o
    join noc_regions as n
        on o.noc = n.noc
    where medal != 'NA'
    group by n.region
    order by num_of_medal desc)
select *
from medal_rank
where ranking between 1 and 5;

-- (14) List down total gold, silver and bronze medals won by each country.
In Postgresql, we can use crosstab function to create pivot table.
crosstab function is part of a PostgreSQL extension called tablefunc.
To call the crosstab function, you must first enable the tablefunc extension by executing the following SQL command:

CREATE EXTENSION TABLEFUNC;

select country,
    coalesce(gold, 0) as gold,
    coalesce(silver, 0) as silver,
    coalesce(bronze, 0) as bronze
from crosstab('select n.region as country,
                medal,
                count(1) as total_medals
                from olympic_history as o
                join noc_regions as n
                    on o.noc = n.noc
                where medal != ''NA''
                group by n.region, medal
                order by n.region, medal',
                'values(''Gold''),(''Silver''),(''Bronze'')')
as final_result(country varchar, gold bigint, silver bigint, bronze bigint)
order by gold desc, silver desc, bronze desc;

-- (14) List down total gold, silver and bronze medals won by each country corresponding to each olympic games.
select games, country,
    coalesce(gold, 0) as gold,
    coalesce(silver, 0) as silver,
    coalesce(bronze, 0) as bronze
from crosstab('select games,
                n.region as country,
                medal,
                count(1) as total_medals
                from olympic_history as o
                join noc_regions as n
                    on o.noc = n.noc
                where medal != ''NA''
                group by games, n.region, medal
                order by games, n.region, medal',
                'values(''Gold''),(''Silver''),(''Bronze'')')
as final_result(games varchar, country varchar, gold bigint, silver bigint, bronze bigint)
order by games, country;

-- (15) In which Sport, India has won highest medals.
with medals_india as
    (select n.region as country, sport, count(medal) as no_of_medals
    from olympic_history as o
    join noc_regions as n
        on o.noc = n.noc
    where medal != 'NA' and n.region = 'India'
    group by n.region, sport
    order by no_of_medals desc)
select sport, no_of_medals
from medals_india
limit 1;

-- (16) Break down all olympic games where India won medal for Hockey and how many medals in each olympic games.
select n.region as country, sport, games, count(medal) as no_of_medals
from olympic_history as o
left join noc_regions as n
    on o.noc = n.noc
where n.region = 'India' and sport = 'Hockey' and medal != 'NA'
group by n.region, sport, games
order by games;