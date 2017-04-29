select bill_id,
case when upper(text_string) like '%PASS%'
  then 'Passed'
when upper(text_string) like '%PASS%' then 'Referred' else 'Referred' end status
from (select bill_id,string_agg(status,' ') text_string from (select bill_id,action_date, text status, max(action_date) over
  (partition by bill_id) as max_thing from actions) tb where tb.max_thing = tb.action_date
group by bill_id) tb1