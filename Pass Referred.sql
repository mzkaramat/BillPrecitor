select count(*),status from (

  SELECT
    tb1.bill_id,
    bills.bill_type,
    bills.originchamber,
    to_char(bills.createdate, 'DD/MM/YYYY'),
   to_char( bills.updatedate, 'DD/MM/YYYY'),
    to_char(bills.introduceddate, 'DD/MM/YYYY'),
   EXTRACT(DAY FROM bills.updatedate - bills.createdate) no_days,
    members.party,
    members.state,
    bill_count_tbl.ac_count,
    CASE WHEN upper(text_string) LIKE upper('%status%') or upper(text_string) LIKE upper('%BecameLaw%')
      THEN 'Passed'

    ELSE 'REFERRED' END status
  FROM (SELECT
          bill_id,
          string_agg(status, ' ') text_string
        FROM (SELECT
                bill_id,
                action_date,
                type                        status,
                max(action_date)
                OVER
                  (
                  PARTITION BY bill_id ) AS max_thing
              FROM actions) tb
        WHERE tb.max_thing = tb.action_date
        GROUP BY bill_id) tb1 INNER JOIN bills ON bills.id = tb1.bill_id
    INNER JOIN (SELECT
                  count(*) ac_count,
                  bill_id
                FROM actions
                GROUP BY bill_id) bill_count_tbl
      ON bill_count_tbl.bill_id = tb1.bill_id
    inner join sponsors
    on sponsors.bill_id = tb1.bill_id
  inner join members
    on members.bioguideid = sponsors.sponsor_id


) tbl group by status
;
