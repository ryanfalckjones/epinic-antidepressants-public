------------------------------------------------------------------------------
-- Preamble: This script contains SQLite syntax for processing the OPIC
-- database. All OPIC tables have been entered into a SQLite database to
-- keep data stored together. For this syntax to work, the path to the database
-- must be correct.
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- CTE ICU_ADM_LAST_DSC:
-- Calculates seconds since last ICU-discharge. 
------------------------------------------------------------------------------

WITH ICU_ADM_LAST_DSC AS (
    SELECT 
        (LopNr * 1000) + Vardtillfalle AS VtfId_LopNr,
        LopNr,
        InskrTidpunkt,
        UtskrTid AS UtskrTidpunkt,
        Avd AS AvdNamn,
        Sjukhus,
        strftime('%s', InskrTidpunkt) - strftime('%s', MAX(UtskrTid) OVER (PARTITION BY LopNr ORDER BY InskrTidpunkt ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)) AS SECS_SINCE_LAST_ICU_DSC
    FROM SIR_BASDATA
),

------------------------------------------------------------------------------
-- CTE ICU_ADM_CONT:
-- Flags and creates a unique ID for each
-- coherent ICU admission to an ICU if no more than 12 hours
-- has passed since a previous ICU discharge.
------------------------------------------------------------------------------

ICU_ADM_CONT AS (
    SELECT
        VtfId_LopNr,
        LopNr,
        InskrTidpunkt,
        UtskrTidpunkt,
        Sjukhus,
        CASE
            WHEN SECS_SINCE_LAST_ICU_DSC IS NULL OR SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1
            ELSE 0
        END AS ADMISSION_FLAG,
        SUM(CASE WHEN SECS_SINCE_LAST_ICU_DSC IS NULL OR SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1 ELSE 0 END) 
        OVER (PARTITION BY LopNr ROWS UNBOUNDED PRECEDING) + LopNr * 100000 AS CONT_ICU_ID
    FROM ICU_ADM_LAST_DSC
),

ICU_ADM_CONT_DATES AS(
    SELECT
      VtfId_LopNr,
      LopNr,
      InskrTidpunkt,
      UtskrTidpunkt,
      CONT_ICU_ID,
      MIN(InskrTidpunkt) OVER (PARTITION BY CONT_ICU_ID) AS CONT_ICU_ADM_DATE,
      MAX(UtskrTidpunkt) OVER (PARTITION BY CONT_ICU_ID) AS CONT_ICU_DSC_DATE
    FROM ICU_ADM_CONT
),

------------------------------------------------------------------------------
-- CTE DESCRIPTIVE_SIR:
-- Creates a CTE with all ICU-data from the database
------------------------------------------------------------------------------

DESCRIPTIVE_SIR AS(
SELECT

    -----------------------------------------
    ------------- DEMOGRAPHICS --------------
    -----------------------------------------
        (LopNr * 1000) + Vardtillfalle AS VtfId_LopNr,
        Kon AS sex, 
        Alder AS age,
        Avd AS sir_icu_name,
        SAPS3Po_ng as SAPS_total_score,
        InskrTidpunkt AS sir_adm_time,
        UtskrTid AS sir_dsc_time,
        
    -----------------------------------------
    --- PHYSIOLOGY AND SEVERITY OF ILLNESS---
    -----------------------------------------

     --- Consciousness level ---
        -- Get SAPS3 values
        SAPS3GCS AS SAPS_GCS,
        SAPS3GCSMototik AS SAPS_GCSm,
        SAPS3RLS AS SAPS_RLS85,

        CASE
            WHEN (SAPS3RLS IS NULL AND SAPS3GCS IS NULL) THEN NULL
            WHEN ((SAPS3RLS IS NOT NULL AND SAPS3RLS IN ('1', '2'))
                    OR (SAPS3GCS IS NOT NULL AND SAPS3GCS IN ('13', '14', '15'))) THEN "I (GCS ≥13)"
            WHEN ((SAPS3RLS IS NOT NULL AND SAPS3RLS IN ('3', '4'))
                    OR (SAPS3GCS IS NOT NULL AND SAPS3GCS IN ('7', '8', '9', '10', '11', '12'))) THEN "II (GCS 7-12)"
            WHEN ((SAPS3RLS IS NOT NULL AND SAPS3RLS IN ('5'))
                    OR (SAPS3GCS IS NOT NULL AND SAPS3GCS IN ('6'))) THEN "III (GCS 6)"
            WHEN ((SAPS3RLS IS NOT NULL AND SAPS3RLS IN ('6'))
                    OR (SAPS3GCS IS NOT NULL AND SAPS3GCS IN ('5'))) THEN "IV (GCS 5)"
            WHEN ((SAPS3RLS IS NOT NULL AND SAPS3RLS IN ('7', '8'))
                    OR (SAPS3GCS IS NOT NULL AND SAPS3GCS IN ('3', '4'))) THEN "V (GCS ≤4)"
            ELSE 0
        END AS sir_consciousness_level
        
    FROM SIR_BASDATA
    WHERE age >= 18
    ),
    --------------------------------------------------------------------------------
-- CONT_DESCRIPTIVE_SIR condenses DESCRIPTIVE_SIR for administrative 
-- ICU admissions to "real" continuous admissions
--------------------------------------------------------------------------------

-- For "first" and "last" occurences create a CTE with row-nubers ordered by adm-time
CONT_DESCRIPTIVE_SIR_RN AS(
  SELECT 
    T.CONT_ICU_ID,
    S.VtfId_LopNr,
    S.sir_adm_time,
    ROW_NUMBER() OVER (PARTITION BY T.CONT_ICU_ID ORDER BY S.sir_adm_time) AS rn,
    ROW_NUMBER() OVER (PARTITION BY T.CONT_ICU_ID ORDER BY S.sir_adm_time DESC) AS last_rn
  FROM DESCRIPTIVE_SIR S
  LEFT JOIN ICU_ADM_CONT T ON S.VtfId_LopNr = T.VtfId_LopNr
  WHERE T.CONT_ICU_ID IS NOT NULL
),

-- Use the CTE created to index first occurrence
CONT_DESCRIPTIVE_SIR_FIRST AS(
  SELECT 
    T.CONT_ICU_ID,
    T.LopNr,
    S.age,
    S.sex,
    S.sir_adm_time,
    S.sir_consciousness_level AS SAPS_consciousness_level, -- I.e. SAPS-based
    S.SAPS_total_score
  FROM DESCRIPTIVE_SIR S
  LEFT JOIN ICU_ADM_CONT T ON S.VtfId_LopNr = T.VtfId_LopNr
  LEFT JOIN CONT_DESCRIPTIVE_SIR_RN RN ON S.VtfId_LopNr = RN.VtfId_LopNr
  WHERE RN.rn = 1
),

-- Add on MAX occurrence for time variable

CONT_DESCRIPTIVE_SIR_MAX_MIN AS(
  SELECT
    T.CONT_ICU_ID,
    MAX(S.sir_dsc_time) AS sir_dsc_time,
    MIN(S.SAPS_RLS85) AS overall_worst_RLS85,
    MIN(S.SAPS_GCS) AS overall_worst_GCS,
    MIN(S.SAPS_GCSm) AS overall_worst_GCSm
  FROM DESCRIPTIVE_SIR S
  LEFT JOIN ICU_ADM_CONT T ON S.VtfId_LopNr = T.VtfId_LopNr
  WHERE T.CONT_ICU_ID IS NOT NULL
  GROUP BY T.CONT_ICU_ID
),

-- Join the sheets to a single CTE for condensed continuous ICU admissions
CONT_DESCRIPTIVE_SIR AS(
  SELECT 
  F.*, 
  MM.sir_dsc_time, 
  MM.overall_worst_RLS85,
  MM.overall_worst_GCS,
  MM.overall_worst_GCSm
  FROM CONT_DESCRIPTIVE_SIR_FIRST F
  LEFT JOIN CONT_DESCRIPTIVE_SIR_MAX_MIN MM ON F.CONT_ICU_ID = MM.CONT_ICU_ID
),

------------------------------------------------------------------------------
-- Processes the cause of death register and produces a CTE
--  - LopNr
--  - DODSDAT_CLEAN is a column of strings with the date of death, some of the 
--      dates are simply years (YYYY) or months (YYYYMM) but all ambiguous formats
--      such as YYYYMM00 or YYYY0000 have been removed.
--  - DODSDAT_DATE is a column of dates for all patients in whom there is a date of 
--  -   death.
--  - DODSDAT_ROUND* rounds the ambiguous death dates to lowest, middle, and 
--    upper bounds.
--  - ERROR_DATE is a column containing a flag for ambiguous dates of death
------------------------------------------------------------------------------

PROCESSED_DORS AS (
  SELECT 
    D.LopNr, 
    
    -- Keep the original date as a string but remove trailing zeroes to enable
    -- future parsing as YMD YM and Y
    CASE
      WHEN SUBSTR(D.DODSDAT, -4, 2) = '00' THEN SUBSTR(D.DODSDAT, 1, 4) 
      WHEN SUBSTR(D.DODSDAT, -4, 2) != '00' AND SUBSTR(D.DODSDAT, -2) = '00' THEN SUBSTR(D.DODSDAT, 1, 6) 
      ELSE D.DODSDAT END AS DODSDAT_CLEAN,
    
    -- Keep correct dates only
    CASE
      WHEN SUBSTR(D.DODSDAT, -2) != '00'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 4) || '-' || SUBSTR(D.DODSDAT, 5, 2) || '-' || SUBSTR(D.DODSDAT, 7, 2))
      ELSE NULL
    END AS DODSDAT_DATE,
    
    -- Keep all dates but for incorrect dates, round to the 1st day of the observation period
    CASE 
      WHEN SUBSTR(D.DODSDAT, -4, 2) = '00'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 4) || '-01-01')
      WHEN SUBSTR(D.DODSDAT, -4, 2) != '00' AND SUBSTR(D.DODSDAT, -2) = '00'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 4) || '-' || SUBSTR(D.DODSDAT, 5, 2) || '-01')
      ELSE DATE(SUBSTR(D.DODSDAT, 1, 4) || '-' || SUBSTR(D.DODSDAT, 5, 2) || '-' || SUBSTR(D.DODSDAT, 7, 2))
    END AS DODSDAT_ROUND_DOWN,
    
    -- Keep all date but for incorrect dates, round up to the last day of observation period
    CASE 
      WHEN SUBSTR(D.DODSDAT, -4, 4) = '0000'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 4) || '-12-31')
      WHEN SUBSTR(D.DODSDAT, -4, 2) != '00' AND SUBSTR(D.DODSDAT, -2) = '00'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 4) || '-' || SUBSTR(D.DODSDAT, 5, 2) || '-01', 'start of month', '+1 month', '-1 day')
      ELSE DATE(SUBSTR(D.DODSDAT, 1, 4) || '-' || SUBSTR(D.DODSDAT, 5, 2) || '-' || SUBSTR(D.DODSDAT, 7, 2))
    END AS DODSDAT_ROUND_UP,
    
        -- Keep all dates but for incorrect dates, round to july 1st for dates with 
        -- YYYY0000 and to the 15th of the month for dates with YYYYMM00
    CASE 
      WHEN SUBSTR(D.DODSDAT, -4, 2) = '00'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 4) || '-07-01')
      WHEN SUBSTR(D.DODSDAT, -4, 2) != '00' AND SUBSTR(D.DODSDAT, -2) = '00'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 4) || '-' || SUBSTR(D.DODSDAT, 5, 2) || '-15')
      ELSE DATE(SUBSTR(D.DODSDAT, 1, 4) || '-' || SUBSTR(D.DODSDAT, 5, 2) || '-' || SUBSTR(D.DODSDAT, 7, 2))
    END AS DODSDAT_ROUND_MID,
    
    -- Add a flag for date with an error in formatting
    CASE WHEN SUBSTR(D.DODSDAT, -2) = '00' THEN 1 ELSE 0 END AS ERROR_DATE
    
  FROM DORS D
)

------------------------------------------------------------------------------
-- Final SELECT-query
------------------------------------------------------------------------------

SELECT 
  S.*,
  D.DODSDAT_CLEAN,
  D.DODSDAT_DATE,
  D.DODSDAT_ROUND_DOWN,
  D.DODSDAT_ROUND_UP,
  D.DODSDAT_ROUND_MID,
  D.ERROR_DATE
FROM CONT_DESCRIPTIVE_SIR S
LEFT JOIN PROCESSED_DORS D ON S.LopNr = D.LopNr