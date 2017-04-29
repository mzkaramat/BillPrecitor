select count(*), status from (SELECT
                                bill_id,
                                bills.bill_type,
                                bills.originchamber,
                                CASE WHEN upper(text_string) LIKE upper('%PASS%')
                                  THEN 'Passed'
                                WHEN upper(text_string) LIKE upper('%Refer%')
                                  THEN 'Referred'
                                ELSE 'Others' END status
                              FROM (SELECT
                                      bill_id,
                                      string_agg(status, ' ') text_string
                                    FROM (SELECT
                                            bill_id,
                                            action_date,
                                            text                        status,
                                            max(action_date)
                                            OVER
                                              (
                                              PARTITION BY bill_id ) AS max_thing
                                          FROM actions) tb
                                    WHERE tb.max_thing = tb.action_date
                                    GROUP BY bill_id) tb1 INNER JOIN bills ON bills.id = tb1.bill_id
) tb group by status;