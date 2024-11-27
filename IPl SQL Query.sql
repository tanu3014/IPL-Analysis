use ipl;

-- Answer to objective 2

SELECT sum(Runs_Scored+COALESCE(Extra_Runs)) AS total_runs
FROM
     batsman_scored b
JOIN 
     extra_runs e on b.Match_Id=e.Match_Id
JOIN 
     player_match pm on b.Match_Id = pm.Match_Id
JOIN 
     matches m on e.Match_Id = m.Match_ID
WHERE
     m.Season_Id =1 AND pm.Team_Id =2;


-- Answer to objective 3
with player_age as 
(select *,Timestampdiff(Year,DOB,'2009-01-01') as AGE
from player)
select count(*) as no_of_players_Season2
from player_age
where AGE>25;

-- Answer to objective 4
select count(*) from matches
where Season_Id = 1 and Match_Winner = 2;

-- answer to objective 5 
select p.Player_Name,m.Season_Id,sum(b.Runs_Scored)*100/count(b.Ball_Id) as strike_rate from batsman_scored b
join player_match pm on b.Match_Id=pm.Match_id
join player p on pm.Player_Id=p.Player_Id
join matches m on b.Match_Id=m.Match_ID
where Season_Id in (6,7,8,9)
group by 1,2
order by strike_rate desc 
limit 10;

-- Answer to objective 6

select p.Player_Name, round(sum(Runs_Scored)/Count(distinct m.Match_Id), 2) as avg_runs 
from batsman_scored b
join player_match pm on b.Match_Id = pm.Match_Id
join player p on pm.Player_Id = p.Player_Id
join matches m on b.Match_Id = m.Match_ID
group by 1;

-- Answer to objective 7 

select p.Player_Name , round(count(Player_Out)/Count(distinct m.Match_Id),2) as avg_wicket 
from wicket_taken w
join player_match pm on w.Match_Id=pm.Match_Id
join player p on pm.Player_Id=p.Player_Id
join matches m on w.Match_Id=m.Match_Id
group by 1
order by 2 desc;

-- Answer to objective 8 

WITH overall_averages AS (
    SELECT AVG(Runs_Scored) AS overall_avg_runs,AVG(Player_Out) AS overall_avg_wickets
    FROM batsman_scored 
    join wicket_taken using (Match_Id)),
    
   player_stats as (
   select p.Player_Id,  round(count(w.Player_Out)/Count(distinct m.Match_Id),2) as avg_wickets, avg(Runs_Scored) avg_runs_scored
   from wicket_taken w
   join batsman_scored b on w.Match_Id=b.Match_Id
   join player_match pm on w.Match_Id=pm.Match_Id
   join matches m on w.Match_Id=m.Match_Id
   join player p on pm.Player_Id=p.Player_Id
   group by 1)
   
select Player_Id,avg_runs_scored,overall_avg_runs,avg_wickets,overall_avg_wickets
from overall_averages oa
join player_stats ps 
ON ps.avg_runs_scored > oa.overall_avg_runs 
AND ps.avg_wickets > oa.overall_avg_wickets;

-- Answer to objective 9
Create table rcb_record (
Venue_Id int primary key,
Venue_name varchar(200),
Wins int default 0,
Losses int default 0);

insert into rcb_record (Venue_Id, Venue_name, Wins, Losses)
 Select Venue_Id , Venue_name,
   sum(case when Match_Winner=2 then 1 else 0 end) as Wins ,
   sum(case when(Match_Winner!=2  and (Team_1=2 or Team_2=2)) then 1 else 0 end) as Losses
   from matches 
   join venue using(Venue_Id)
   where Team_1= 2 or Team_2=2
   group by 1,2
   ;
   -- Answer to objective 10
   
   select b.Bowling_skill,Count(distinct p.Player_Id) as no_of_bowlers, Sum(Player_Out) as total_wickets, Round(Sum(Player_Out)/Count(distinct p.Player_Id),0) as avg_wickets_taken_per_bowler 
   from matches m 
   join wicket_taken w on m.Match_Id=w.Match_Id
   join ball_by_ball bb ON w.Match_Id = bb.Match_Id 
                     AND w.Over_Id = bb.Over_Id 
                     AND w.Ball_Id = bb.Ball_Id
	join player p on bb.Bowler=p.Player_Id
   join player_match pm on pm.Player_Id=p.Player_Id and  pm.Match_Id=m.Match_Id 
   join bowling_style b on p.Bowling_skill=b.Bowling_Id
   group by 1;
   
   -- query 2 for question 10
   select b.Bowling_skill,Count(distinct p.Player_Id) as no_of_bowlers, Sum(Player_Out) as total_wickets, Round(Sum(Player_Out)/Count(distinct p.Player_Id),0) as avg_wickets_taken_per_bowler
   from matches m 
   join wicket_taken w on m.Match_Id=w.Match_Id
   join player_match pm on m.Match_Id=pm.Match_Id
   join player p on pm.Player_Id=p.Player_Id
   join bowling_style b on p.Bowling_skill=b.Bowling_Id
   group by 1
   order by 4 desc;
   
   -- answer to objective 11
   
   with team_performance as
   (select m.Season_Id,t.Team_Name, sum(bs.Runs_Scored) as total_runs, 
     sum(w.Player_Out) as total_wickets
 from team t join matches m on t.Team_Id in (m.Team_1,m.Team_2)
 left join ball_by_ball b on b.Match_Id=m.Match_Id and t.Team_Id=b.Team_Batting
 left join batsman_scored bs on bs.Match_Id=b.Match_Id and bs.Over_Id=b.Over_Id and bs.Ball_Id=b.Ball_Id and bs.Innings_No=b.Innings_No
 left join wicket_taken w on w.Match_Id=b.Match_Id and w.Over_Id=b.Over_Id and w.Ball_Id=b.Ball_Id and w.Innings_No=b.Innings_No
 group by 1,2,3),
 
 performance_comparison as (
 select Season_Id,Team_Name, total_runs,total_wickets, lag(total_runs) over(partition by Team_Id order by Season_Id) as prev_year_runs,
 lag(total_wickets) over(partition by Team_Id order by Season_Id)as prev_year_wickets 
 from team_performance)
 
 select Season_Id,Team_Name,total_runs, prev_year_runs,total_wickets,prev_year_wickets, 
 case 
    when total_runs > prev_year_runs and total_wickets > prev_year_wickets then 'Better' 
    when total_runs < prev_year_runs and total_wickets < prev_year_wickets then 'Poor'
    else 'Mixed'
 end  as  Performance_status from performance_comparison 
 where prev_year_runs is not null and prev_year_wickets is not null 
 order by Team_Name,Season_Id;
 
 -- Objective answer 12
 
 WITH team_performance AS (
    SELECT  
        m.Season_Id, t.Team_Id,t.Team_Name, 
        COUNT(DISTINCT m.Match_Id) AS matches_played,
        COUNT(b.Innings_No) AS total_innings, 
        SUM(bs.Runs_Scored) AS total_runs, 
        COUNT(w.Player_Out) AS total_wickets,
        SUM(CASE WHEN bs.Runs_Scored = 4 THEN 1 ELSE 0 END) AS fours,
        SUM(CASE WHEN bs.Runs_Scored = 6 THEN 1 ELSE 0 END) AS sixes, 
        SUM(CASE WHEN bs.Runs_Scored = 0 THEN 1 ELSE 0 END) AS dot_balls, 
        COUNT(b.Ball_Id) AS balls_faced,
		SUM(CASE WHEN b.Over_Id >= 16 THEN bs.Runs_Scored ELSE 0 END) AS Death_Over_Runs,
		SUM(CASE WHEN b.Over_Id < 7 THEN bs.Runs_Scored ELSE 0 END) AS Powerplay_Runs,
        COUNT(DISTINCT CASE WHEN m.Win_Type = t.Team_Id THEN m.Match_Id END) AS matches_won
        
    FROM team t 
    JOIN matches m ON t.Team_Id IN (m.Team_1, m.Team_2)
    LEFT JOIN ball_by_ball b ON b.Match_Id = m.Match_Id
    LEFT JOIN batsman_scored bs ON b.Match_Id = bs.Match_Id AND b.Over_Id = bs.Over_Id AND b.Ball_Id = bs.Ball_Id AND b.Innings_No = bs.Innings_No
    LEFT JOIN wicket_taken w ON w.Match_Id = b.Match_Id AND w.Over_Id = b.Over_Id AND w.Ball_Id = b.Ball_Id AND w.Innings_No = b.Innings_No
    GROUP BY 1, 2, 3
),
performance_comparison AS (
    SELECT 
        Season_Id, Team_Id,Team_Name,matches_played,total_innings, total_runs,total_wickets,fours,sixes,dot_balls,balls_faced,matches_won,Powerplay_Runs,Death_Over_Runs,
        round((total_runs * 6.0) / balls_faced,2) AS Economy_Rate ,
        ROUND(total_runs*100/balls_faced , 2) AS strike_rate,
        ROUND(total_runs/matches_played, 2) AS avg_runs_per_match,
        ROUND(total_wickets/matches_played, 2) AS avg_wickets_per_match,
        ROUND(dot_balls*100/matches_played , 2) AS dot_balls_percentage,
        LAG(round(total_runs/matches_played,2)) OVER (PARTITION BY Team_Id ORDER BY Season_Id) AS prev_year_runs, 
        LAG(round(total_wickets/matches_played,2)) OVER (PARTITION BY Team_Id ORDER BY Season_Id) AS prev_year_wickets,
        
        LAG(ROUND(total_runs*100/balls_faced, 2)) OVER (PARTITION BY Team_Id ORDER BY Season_Id) AS prev_year_strike_rate 
    FROM team_performance
)
SELECT 
    Season_Id, Team_Name, matches_played,total_runs,total_wickets,fours,sixes,strike_rate,avg_runs_per_match,avg_wickets_per_match,
    prev_year_runs, prev_year_wickets, prev_year_strike_rate,Economy_Rate, Death_Over_Runs, Powerplay_Runs,
   CASE
        WHEN total_runs > prev_year_runs AND total_wickets > prev_year_wickets AND strike_rate > prev_year_strike_rate THEN 'Significantly Better'
        WHEN total_runs > prev_year_runs AND total_wickets > prev_year_wickets THEN 'Better'
        WHEN total_runs < prev_year_runs AND total_wickets < prev_year_wickets AND strike_rate < prev_year_strike_rate THEN 'Significantly Worse'
        WHEN total_runs < prev_year_runs AND total_wickets < prev_year_wickets THEN 'Worse'
        WHEN strike_rate > prev_year_strike_rate THEN 'Improved strike_rate '
        ELSE 'Mixed'
    END AS performance_status 
FROM performance_comparison 
WHERE prev_year_runs IS NOT NULL AND prev_year_wickets IS NOT NULL 
ORDER BY Team_Name, Season_Id;


   
   -- Answer to objective no 13
   Select p.Player_Id,Player_Name,Venue_Name,Round(avg(Player_Out),2) as average_wickets,
		  dense_rank() over(partition by Venue_Name order by avg(Player_Out) desc) as `rank`
   from wicket_taken w
   join matches m on w.Match_Id=m.Match_Id
   join venue v on v.Venue_Id=m.Venue_Id
   join player_match pm on m.Match_Id=pm.Match_Id
   join player p on pm.Player_ID=p.Player_Id
   group by 1,2,3;
   
   -- Answer to objective 14
   
   With Player_Performance as (
   select p.Player_Id,Player_Name,Season_Id,sum(Runs_Scored) as total_runs, count( distinct m.Match_Id) as matches_played
   from matches m 
   join player_match pm on pm.Match_Id=m.Match_Id
   join player p on p.Player_Id=pm.Player_Id
   join batsman_scored b on m.Match_Id=b.Match_Id
   Group by 1,2,3),
   
   consistent_players as (
   select Player_Id,Player_Name, Min(total_runs) as min_runs ,Max(total_runs) as max_runs,
   avg(total_runs) as avg_runs_per_season,round(stddev(total_runs),2) as stddev_runs
   from Player_Performance
   group by 1,2)
   
   select * from consistent_players
   where avg_runs_per_season>(select avg(total_runs) from Player_Performance)
   and stddev_runs<50
   order by avg_runs_per_season desc;
   
   -- Answer to objective 15
   
   With Player_Performance_by_Venue as 
   (select Player_Name, Venue_Name, Sum(Runs_Scored) as total_runs_at_venue,
		   Count(distinct m.Match_Id) as matches_played,round(sum(Runs_Scored)/Count(distinct m.Match_Id),2) as avg_runs_at_venue
   from batsman_scored b
   join player_match pm on b.Match_ID=pm.Match_ID
   join player p on  pm.Player_Id=p.Player_Id
   join matches m on b.Match_Id=m.Match_Id
   join venue v on  m.Venue_Id=v.Venue_Id
   group by 1,2
   having Count(distinct m.Match_Id)>=3),
   
  Player_Overall_Performance as ( 
  select p.Player_Name,sum(Runs_Scored) as overall_runs,Count(distinct m.Match_Id), 
							round(sum(Runs_Scored)/Count(distinct m.Match_Id), 2) as overall_avg_runs 
   from batsman_scored b
   join player_match pm on b.Match_Id = pm.Match_Id
   join player p on pm.Player_Id = p.Player_Id
   join matches m on b.Match_Id = m.Match_ID
   group by 1)
   
  Select pv.Player_Name,Venue_name,total_runs_at_venue,avg_runs_at_venue,overall_runs,overall_avg_runs,
			(avg_runs_at_venue-overall_avg_runs) as performance_difference
   from Player_performance_by_Venue pv
   join Player_Overall_Performance po on pv.Player_Name=po.Player_Name
   order by performance_difference desc
   limit 10;
   
   
   
  

   -- Subjective answer 

  -- Subjective answer 1
   select Venue_Name,Toss_Name,Sum(case when Toss_Winner=Match_Winner then 1 else 0 end ) as wins, Sum(case when Toss_Winner!=Match_Winner then 1 else 0 end) as Losses
   from matches m
   join venue v on m.Venue_Id=v.Venue_Id
   join toss_decision t on m.Toss_Decide=t.Toss_id
   group by 1,2
   order by 1;


 -- Subjective answer 2
 with batsman_bowler_performance as (
select p.Player_Id,p.Player_Name,
    COUNT(DISTINCT m.Match_Id) AS Matches_Played,
    SUM(bs.Runs_Scored) AS Total_Runs,
   round(SUM(bs.Runs_Scored) /COUNT(w.Player_Out),2) AS Batting_Average,
    round(SUM(bs.Runs_Scored) * 100.0 / COUNT(b.Ball_Id),2)AS Strike_Rate,
    COUNT(w.Player_Out) AS Wickets_Taken,
     SUM(bs.Runs_Scored + COALESCE(Extra_Runs, 0)) AS Runs_Conceded,
    round((SUM(bs.Runs_Scored + COALESCE(Extra_Runs, 0))/COUNT(w.Player_Out)),2) AS Bowling_Average,
    round((SUM(bs.Runs_Scored) * 6.0) / COUNT(b.Ball_Id),2) AS Economy_Rate 
    from player p join player_match pm on p.Player_Id=pm.Player_Id
    join matches m on m.Match_id=pm.Match_Id
    join ball_by_ball b on b.Match_Id=m.Match_Id
    LEFT JOIN extra_runs e ON m.Match_Id = e.Match_Id AND b.Ball_Id = e.Ball_Id
    left join batsman_scored bs on bs.Match_Id=b.Match_Id and bs.Over_Id=b.Over_Id and bs.Ball_Id=b.Ball_Id and bs.Innings_No=b.Innings_No
    left join wicket_taken w on  bs.Match_Id=w.Match_Id and bs.Over_Id=w.Over_Id and bs.Ball_Id=w.Ball_Id and bs.Innings_No=w.Innings_No
    group by 1,2 having Matches_Played>10 )
select Player_Id, Player_name,Total_Runs, Batting_Average,Strike_Rate, Wickets_Taken,Runs_Conceded,Bowling_Average,Economy_Rate 
from batsman_bowler_performance
order by Total_Runs desc, Wickets_Taken desc
limit 20;



-- subjective answer 4

SELECT 
    p.Player_Name,
    COUNT(DISTINCT m.Match_Id) AS Matches_Played,
    ROUND(SUM(bs.Runs_Scored) * 1.0 / NULLIF(COUNT(DISTINCT CASE WHEN b.Striker= p.Player_Id THEN m.Match_Id END), 0), 2) AS Batting_Average,
    COUNT(w.Player_Out) AS Wickets_Taken,
    ROUND(SUM(CASE WHEN b.Bowler = p.Player_Id THEN bs.Runs_Scored ELSE 0 END) * 1.0 / NULLIF(COUNT(w.Player_Out), 0), 2) AS Bowling_Average,
    ROUND((
        (SUM(bs.Runs_Scored) * 1.0 / NULLIF(COUNT(DISTINCT CASE WHEN b.Striker = p.Player_Id THEN m.Match_Id END), 0)) +
        (COUNT(w.Player_Out) * 25.0 / NULLIF(COUNT(DISTINCT CASE WHEN b.Bowler= p.Player_Id THEN m.Match_Id END), 0))
    ), 2) AS All_Rounder_Score
FROM 
    player p
LEFT JOIN 
    ball_by_ball b ON p.Player_Id IN (b.Striker, b.Non_Striker, b.Bowler)
LEFT JOIN 
    batsman_scored bs ON b.Match_Id = bs.Match_Id AND b.Over_Id = bs.Over_Id AND b.Ball_Id = bs.Ball_Id
LEFT JOIN 
    wicket_taken w ON b.Match_Id = w.Match_Id AND b.Over_Id = w.Over_Id AND b.Ball_Id = w.Ball_Id
JOIN 
    matches m ON b.Match_Id = m.Match_Id
GROUP BY 
    p.Player_Id, p.Player_Name
HAVING 
    Matches_Played >= 20 AND Wickets_Taken >= 10 AND Batting_Average > 20
ORDER BY 
    All_Rounder_Score DESC
LIMIT 20;

-- Subjective answer 5

with player_matches as (select m.Match_Id, t.Team_Id, t.Team_Name, p.Player_Id,p.Player_Name,m.Match_Winner,m.Win_Margin, 
						CASE WHEN w.Win_Type='runs' then 1 else 0 end as Won_Batting_First, 
						CASE WHEN w.Win_Type='wickets' then 1 else 0 end as  Won_Bowling_First 
from player_match pm 
join player p on pm.Player_Id=p.Player_Id
join matches m on pm.Match_Id=m.Match_Id
join team t on t.Team_Id in (m.Team_1,m.Team_2)
join win_by  w on w.Win_Id=m.Win_Type),

team_performance as (select pm1.Team_Id, pm1.Team_Name, pm1.Player_Id,pm1.Player_Name,
					count(distinct pm1.Match_Id) as matches_played, 
                    sum(case when pm1.Match_Winner =pm1.Team_Id then 1 else 0 END) AS Matches_Won,
					avg(case when pm1.Match_Winner =pm1.Team_Id  then pm1.Win_Margin else null end) as avg_win_margin,
					sum(pm1.Won_Batting_First) as wins_batting_first, sum(pm1.Won_Bowling_First) as wins_bowling_first 
from  player_matches pm1 group by 1,2,3,4),

team_overall_performance as (select t1.Team_Id, count(distinct m1.Match_Id) as total_matches, sum(case when m1.Match_Winner=t1.Team_Id then 1 else 0 end) as total_wins
from matches m1
join team t1 on t1.Team_Id in (m1.Team_1,m1.Team_2) 
group by t1.Team_Id)

SELECT tp.Team_Id,tp.Team_Name, tp.Player_Name, tp.matches_played,tp.Matches_Won,
round(tp.Matches_Won * 100.0/tp.matches_played,2) as win_percentage, 
round(tp.avg_win_margin ,2) as avg_win_margin,tp.wins_batting_first,tp.wins_bowling_first,top.total_matches,top.total_wins, 
round(top.total_wins * 100.0/top.total_matches, 2) as team_overall_win_percentage,
round((tp.Matches_Won * 100.0/tp.matches_played)-(top.total_wins * 100.0 /top.total_matches),2) as win_percentage_difference
from team_performance tp 
join team_overall_performance top on tp.Team_Id=top.Team_Id
where tp.matches_played >=10 
order by win_percentage_difference desc, win_percentage desc
limit 20;



-- Objective answer 6

with player_performance as
 (select p.Player_Id, p.Player_Name, t.Team_Name,
 COUNT(DISTINCT m.Match_Id) AS Matches_Played, 
 SUM(bs.Runs_Scored) AS Total_Runs, AVG(bs.Runs_Scored) AS Batting_Average,
SUM(bs.Runs_Scored) * 100.0 / COUNT(b.Ball_Id) AS Strike_Rate,
COUNT(CASE WHEN bs.Runs_Scored >= 50 THEN 1 END) AS Fifty_Plus_Scores,
COUNT(w.Player_Out) AS Wickets,
SUM(CASE WHEN b.Bowler = p.Player_Id THEN bs.Runs_Scored ELSE 0 END) * 6.0 / COUNT(CASE WHEN b.Bowler = p.Player_Id THEN 1 END) AS Economy_Rate, 
(SUM(bs.Runs_Scored) / COUNT(DISTINCT m.Match_Id)) + (COUNT(w.Player_Out) * 20.0 / COUNT(DISTINCT m.Match_Id)) AS All_Rounder_Index,
SUM(CASE WHEN b.Over_Id >= 16 AND b.Bowler = p.Player_Id THEN bs.Runs_Scored ELSE 0 END) * 6.0 / COUNT(CASE WHEN b.Over_Id >= 16 AND b.Bowler = p.Player_Id THEN 1 END) AS Death_Over_Economy
from ball_by_ball b left join batsman_scored bs on b.Match_Id=bs.Match_Id and b.Over_Id=bs.Over_Id and b.Ball_Id = bs.Ball_Id and b.Innings_No=bs.Innings_No
left join wicket_taken w on b.Match_Id=w.Match_Id and b.Over_Id=w.Over_Id and b.Ball_Id=w.Ball_Id and b.Innings_No=w.Innings_No
join matches m on b.Match_Id=m.Match_Id join  player p on p.Player_Id in (b.Striker,b.Non_Striker,b.Bowler) join team t on t.Team_Id in (m.Team_1,m.Team_2)
group by p.Player_Id,p.Player_Name, t.Team_Name having Matches_Played >=10),

player_rankings as (select *, 
dense_rank() over(order by Total_Runs DESC) AS Batting_Rank,
dense_rank() OVER(ORDER BY Wickets DESC) AS Bowling_Rank,
dense_rank() OVER(ORDER BY All_Rounder_Index DESC) AS All_Rounder_Rank,
dense_rank() OVER(ORDER BY Death_Over_Economy) AS Death_Bowler_Rank
FROM player_performance)
select pr.Player_Name,pr.Matches_Played, pr.Total_Runs,
 ROUND(pr.Batting_Average, 2) AS Batting_Average,
    ROUND(pr.Strike_Rate, 2) AS Strike_Rate,
    pr.Fifty_Plus_Scores,
    pr.Wickets,
    ROUND(pr.Economy_Rate, 2) AS Economy_Rate,
    ROUND(pr.All_Rounder_Index, 2) AS All_Rounder_Index,
    ROUND(pr.Death_Over_Economy, 2) AS Death_Over_Economy,
    pr.Batting_Rank,
    pr.Bowling_Rank,
    pr.All_Rounder_Rank,
    pr.Death_Bowler_Rank,
    CASE 
        WHEN pr.Batting_Rank <= 20 THEN 'Top Batsman'
        WHEN pr.Bowling_Rank <= 20 THEN 'Top Bowler'
        WHEN pr.All_Rounder_Rank <= 10 THEN 'Top All-Rounder'
        WHEN pr.Death_Bowler_Rank <= 10 THEN 'Top Death Bowler'
        ELSE 'Promising Talent'
    END AS Player_Category
    FROM player_rankings pr
    WHERE 
    (pr.Batting_Rank <= 20 OR pr.Bowling_Rank <= 20 OR pr.All_Rounder_Rank <= 10 OR pr.Death_Bowler_Rank <= 10)
    AND pr.Player_Name NOT IN (SELECT Player_Name FROM player WHERE Player_Id IN (SELECT Player_Id FROM player_match WHERE Team_Name = 'RCB'))
    ORDER BY 
    CASE 
        WHEN pr.Death_Bowler_Rank <= 10 THEN 1  -- Prioritize death bowlers
        WHEN pr.All_Rounder_Rank <= 10 THEN 2   -- Then all-rounders
        WHEN pr.Batting_Rank <= 20 THEN 3       -- Then top batsmen
        WHEN pr.Bowling_Rank <= 20 THEN 4       -- Then top bowlers
        ELSE 5
    END,
 pr.All_Rounder_Index DESC
LImit 20;

-- Subjective answer 7

WITH match_scores AS (select m.Match_Id,t.Team_Name AS Batting_Team,SUM(bs.Runs_Scored) AS Total_Runs, v.Venue_Name,c.City_Name, co.Country_Name, td.Toss_Name, m.Season_Id, m.Outcome_type,
                       SUM(CASE WHEN b.Over_Id >= 16 THEN bs.Runs_Scored ELSE 0 END) AS Death_Over_Runs,
                       SUM(CASE WHEN b.Over_Id < 7 THEN bs.Runs_Scored ELSE 0 END) AS Powerplay_Runs
FROM matches m 
join player_match pm on m.Match_Id=pm.Match_Id join ball_by_ball b on m.Match_Id=b.Match_Id
join batsman_scored bs on b.Match_Id=bs.Match_Id and b.Over_Id=bs.Over_Id and b.Ball_Id=bs.Ball_Id
join toss_decision td on td.Toss_Id=m.Toss_Winner
join team t on pm.Team_Id=t.Team_Id join venue v on m.Venue_Id=v.Venue_Id join city c on v.City_Id=c.City_Id
join country co on c.Country_id=co.Country_Id where bs.Runs_Scored is not null 
group by  m.Match_Id, t.Team_Name, v.Venue_Name, c.City_Name, co.Country_Name, td.Toss_Name, m.Season_Id, m.Outcome_type 
having sum(bs.Runs_Scored) >300),

venue_analysis as 
           ( select v.Venue_Name,c.City_Name, co.Country_Name, AVG(ms.Total_Runs) AS Avg_Runs,AVG(ms.Death_Over_Runs) AS Avg_Death_Over_Runs,
           AVG(ms.Powerplay_Runs) AS Avg_Powerplay_Runs,
           COUNT(ms.Match_Id) AS High_Scoring_Matches from  match_scores ms join venue v on ms.Venue_Name=v.Venue_Name 
join city c on c.City_Id=v.City_Id 
join  country co on c.Country_id=co.Country_id
group by v.Venue_Name,c.City_Name, co.Country_Name),

team_performance as 
(select t.Team_Name,AVG(ms.Total_Runs) AS Avg_Team_Runs, AVG(ms.Death_Over_Runs) AS Avg_Team_Death_Over_Runs, 
AVG(ms.Powerplay_Runs) AS Avg_Team_Powerplay_Runs, COUNT(ms.Match_Id) AS High_Scoring_Matches
from match_scores ms join team t on ms.Batting_Team=t.Team_Name 
group by t.Team_Name)

select va.Venue_Name,
    va.City_Name,va.Country_Name,va.Avg_Runs AS Avg_Venue_Runs,va.Avg_Death_Over_Runs AS Avg_Venue_Death_Over_Runs,
    va.Avg_Powerplay_Runs AS Avg_Venue_Powerplay_Runs,va.High_Scoring_Matches AS High_Scoring_Matches_Venue,
    tp.Team_Name,tp.Avg_Team_Runs AS Avg_Team_Runs,tp.Avg_Team_Death_Over_Runs AS Avg_Team_Death_Over_Runs,
    tp.Avg_Team_Powerplay_Runs AS Avg_Team_Powerplay_Runs,tp.High_Scoring_Matches AS High_Scoring_Matches_Team
from venue_analysis va JOIN team_performance tp on tp.Avg_Team_Runs > 300
order by va.High_Scoring_Matches DESC,tp.Avg_Team_Runs DESC;



-- Subjective answer 8 
with team_performance as (SELECT t.Team_Name,v.Venue_Name,m.Match_Id,SUM(bs.Runs_Scored) AS Total_Runs,COUNT(w.Player_Out) AS Wickets_Taken,
SUM(CASE WHEN b.Over_Id >= 16 THEN bs.Runs_Scored ELSE 0 END) * 6.0 / COUNT(CASE WHEN b.Over_Id >= 16 THEN 1 END) AS Death_Over_Economy,
case when v.Venue_Name='M Chinnaswamy Stadium' then 'Home' else 'Away' end as 'Ground_type',
case when m.Outcome_type=2 then 1 else 0 end as win
from matches m join player_match pm on m.Match_Id=pm.Match_id
join team t on pm.Team_Id=t.Team_Id
join ball_by_ball b on m.Match_Id = b.Match_Id 
left join  batsman_scored bs on b.Match_Id = bs.Match_Id AND b.Over_Id = bs.Over_Id AND b.Ball_Id = bs.Ball_Id
left join wicket_taken w on b.Match_Id = w.Match_Id AND b.Over_Id = w.Over_Id AND b.Ball_Id = w.Ball_Id
join venue v on m.Venue_Id=v.Venue_Id where t.Team_Name='Royal Challengers Bangalore' group by t.Team_Name,v.Venue_Name,m.Match_Id),
team_home_away_stats AS (
    SELECT 
        Ground_Type,
        COUNT(DISTINCT Match_Id) AS Matches_Played,
        SUM(Win) AS Wins,
        ROUND(SUM(Win) * 100.0 / COUNT(DISTINCT Match_Id), 2) AS Win_Percentage,
        AVG(Total_Runs) AS Avg_Runs_Scored,
        AVG(Wickets_Taken) AS Avg_Wickets_Taken,
        ROUND(AVG(Death_Over_Economy), 2) AS Avg_Death_Over_Economy
    FROM 
        team_performance
    GROUP BY 
        Ground_Type)
SELECT 
    Ground_Type AS Venue_Type,
    Matches_Played,
    Wins,
    Win_Percentage,
    Avg_Runs_Scored,
    Avg_Wickets_Taken,
    Avg_Death_Over_Economy
FROM 
    team_home_away_stats;



       
	-- subjective ans 9
    
    WITH rcb_performance AS (
    SELECT 
        m.Match_Id,
        m.Season_Id,
        m.Match_Winner,
        SUM(CASE WHEN (m.Team_1 = 2 OR m.Team_2 = 2) AND bb.Striker = 2 THEN b.Runs_Scored ELSE 0 END) AS Runs_Scored,
        SUM(CASE WHEN (m.Team_1 = 2 OR m.Team_2 = 2) AND bb.Striker != 2 THEN b.Runs_Scored ELSE 0 END) AS Runs_Conceded
    FROM 
        matches m
    JOIN 
        ball_by_ball bb ON m.Match_Id = bb.Match_Id
    JOIN 
        batsman_scored b ON bb.Match_Id = b.Match_Id AND bb.Over_Id = b.Over_Id AND bb.Ball_Id = b.Ball_Id
    WHERE 
        m.Team_1 = 2 OR m.Team_2 = 2
    GROUP BY 
        m.Match_Id, m.Season_Id, m.Match_Winner
)
SELECT
    Season_Id,
    COUNT(CASE WHEN Match_Winner = 2 THEN 1 END) AS Wins,
    COUNT(CASE WHEN Match_Winner != 2 AND Match_Winner IS NOT NULL THEN 1 END) AS Losses,
    ROUND(AVG(Runs_Scored), 2) AS Avg_Runs_Scored,
    ROUND(AVG(Runs_Conceded), 2) AS Avg_Runs_Conceded
FROM 
    rcb_performance
GROUP BY 
    Season_Id
ORDER BY 
    Season_Id;        
    
    

-- Subjective answer 11 

update `Match `
Set Opponent_Team = 'Delhi_Daredevils'
where Opponent_Team = 'Delhi_Capitals';

select * from matches
where Opponent_Team = 'Delhi_Daredevils'  
    
       




   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    
   
   











