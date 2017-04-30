SELECT
        type,
        avg(
            CASE WHEN type = 'BecameLaw'
              THEN
                next_row - action_date
            ELSE 0 END) became_law,
        avg(
            CASE WHEN type = 'Calendars'
              THEN
                next_row - action_date
            ELSE 0 END) Calendars,
        avg(
            CASE WHEN type = 'Committee'
              THEN
                next_row - action_date
            ELSE 0 END) Committee,
        avg(
            CASE WHEN type = 'Discharge'
              THEN
                next_row - action_date
            ELSE 0 END) Discharge,
        avg(
            CASE WHEN type = 'Floor'
              THEN
                next_row - action_date
            ELSE 0 END) Floor,
        avg(
            CASE WHEN type = 'IntroReferral'
              THEN
                next_row - action_date
            ELSE 0 END) IntroReferral,
        avg(
            CASE WHEN type = 'NotUsed'
              THEN
                next_row - action_date
            ELSE 0 END) NotUsed,
        avg(
            CASE WHEN type = 'President'
              THEN
                next_row - action_date
            ELSE 0 END) President,
        avg(
            CASE WHEN type = 'ResolvingDifferences'
              THEN
                next_row - action_date
            ELSE 0 END) ResolvingDifferences,
        avg(
            CASE WHEN type = 'Veto'
              THEN
                next_row - action_date
            ELSE 0 END) Veto
      FROM (SELECT
              bill_id,
              action_date,
              type,
              lead(action_date, 1)
              OVER (
                PARTITION BY bill_id
                ORDER BY action_date ) next_row,
              lead(type, 1)
              OVER (
                PARTITION BY bill_id
                ORDER BY action_date ) next_type
            FROM actions
            ORDER BY bill_id, action_date) tb1 INNER JOIN bills ON bills.id = tb1.bill_id
      WHERE next_type IS NOT NULL
      GROUP BY type, next_type