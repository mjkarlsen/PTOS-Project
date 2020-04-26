

#Classification of Male or Female (pg. 17)
gender_func <- function(col) {
  col_value <-  case.(col == 1, "M",
                      col == 2, "F",
                      default = "U")
  return(col_value)
}

#Classification of race (pg. 17)
race_func <- function(col) {
  col_value <-  case.(col == 1, "White",
                      col == 2, "Black",
                      col == 3, "Hispanic", 
                      col == 4, "Asian", 
                      col == 5, "Other", 
                      default = "Unknown")
  return(col_value)
}

#Classification of counties in Pennsylvania (pg. 112)
county_func <- function(col) {
  col_value <- case.(col == 1 , 'Adams',
                     col == 2 , 'Allegheny',
                     col == 3 , 'Armstrong',
                     col == 4 , 'Beaver',
                     col == 5 , 'Bedford',
                     col == 6 , 'Berks',
                     col == 7 , 'Blair',
                     col == 8 , 'Bradford',
                     col == 9 , 'Bucks',
                     col == 10 , 'Butler',
                     col == 11 , 'Cambria',
                     col == 12 , 'Cameron',
                     col == 13 , 'Carbon',
                     col == 14 , 'Centre',
                     col == 15 , 'Chester',
                     col == 16 , 'Clarion',
                     col == 17 , 'Clearfield',
                     col == 18 , 'Clinton',
                     col == 19 , 'Columbia',
                     col == 20 , 'Crawford',
                     col == 21 , 'Cumberland',
                     col == 22 , 'Dauphin',
                     col == 23 , 'Delaware',
                     col == 24 , 'Elk',
                     col == 25 , 'Erie',
                     col == 26 , 'Fayette',
                     col == 27 , 'Forest',
                     col == 28 , 'Franklin',
                     col == 29 , 'Fulton',
                     col == 30 , 'Greene',
                     col == 31 , 'Huntingdon',
                     col == 32 , 'Indiana',
                     col == 33 , 'Jefferson',
                     col == 34 , 'Juniata',
                     col == 35 , 'Lackawanna',
                     col == 36 , 'Lancaster',
                     col == 37 , 'Lawrence',
                     col == 38 , 'Lebanon',
                     col == 39 , 'Lehigh',
                     col == 40 , 'Luzerne',
                     col == 41 , 'Lycoming',
                     col == 42 , 'McKean',
                     col == 43 , 'Mercer',
                     col == 44 , 'Mifflin',
                     col == 45 , 'Monroe',
                     col == 46 , 'Montgomery',
                     col == 47 , 'Montour',
                     col == 48 , 'Northampton',
                     col == 49 , 'Northumberland',
                     col == 50 , 'Perry',
                     col == 51 , 'Philadelphia',
                     col == 52 , 'Pike',
                     col == 53 , 'Potter',
                     col == 54 , 'Schuylkill',
                     col == 55 , 'Snyder',
                     col == 56 , 'Somerset',
                     col == 57 , 'Sullivan',
                     col == 58 , 'Susquehanna',
                     col == 59 , 'Tioga',
                     col == 60 , 'Union',
                     col == 61 , 'Venango',
                     col == 62 , 'Warren',
                     col == 63 , 'Washington',
                     col == 64 , 'Wayne',
                     col == 65 , 'Westmoreland',
                     col == 66 , 'Wyoming',
                     col == 67 , 'York', 
                     default = "Unknown")
  
  return(col_value)
}

#Classification of type of injury (pg. 20)
injury_func <- function(col) {
  col_value <-  case.(col == 1, "Blunt",
                      col == 2, "Penetrating",
                      col == 3, "Burn", 
                      col == 4, "Skin Disease", 
                      default = "Unknown")
  return(col_value)
}

#Any action that disentagles or frees from entrapments (pg. 23)
extricated_func <- function(col) {
  col_value <-  case.(col == 1, "Yes", 
                      col == 2, "No", 
                      default = "Unknown")
  return(col_value)
}

#Method used to transport the patient to the trauma center (pg. 24)
transportation_func <- function(col) {
  col_value <-  case.(col == 1, "Ambulance",
                      col == 2, "Helicopter",
                      col == 3, "Ambulance/Helicopter",
                      col == 4, "Police",
                      col == 5, "Fire Rescue",
                      col == 6, "Private Vehicle ",
                      col == 7, "Walk-In",
                      col == 8, "NA",
                      col == 9, "Quick Response Service",
                      default = "Unknown")
  return(col_value)
}

#Record of drugs patient tested positive at referring hospital (pg. 42)
drug_func <- function(col) {
  col_value <-  case.(col == 0, "Not Tested",
                      col == 1, "None",
                      col == 2, "Cocaine",
                      col == 3, "PCP",
                      col == 4, "Benzodiazepines",
                      col == 5, "Barbiturates",
                      col == 6, "Narcotics",
                      col == 7, "Amphetamines",
                      col == 8, "Mariguana",
                      col == 9, "Tricycloids", 
                      default = "Unknown")
  return(col_value)
}

#Attending surgeon specialty who was present in OR (pg. 55)
attending_surgeon_func <- function(col) {
  col_value <-  case.(col == 1, 'Trauma/General',
                      col == 2, 'Neurosurgery ',
                      col == 3, 'Orthopedic',
                      col == 4, 'Cardiac',
                      col == 5, 'OBGY',
                      col == 6, 'Ophthalmic',
                      col == 7, 'Oral/Maxillofacial',
                      col == 8, 'Otorhinolaryngologic',
                      col == 9, 'Pediatric',
                      col == 10, 'Plastic',
                      col == 11, 'Thoracic',
                      col == 12, 'Urologic',
                      col == 13, 'Burn',
                      col == 77, 'Other',
                      default = "Unknown")
  return(col_value)
}

# Assessment of the stimulus required to induce eye opening, 
# assessed within 30 minutes or less of ED/hospital arrival  (pg. 67)
eye_resp_func <- function(col) {
  col_value <-  case.(col == 1, "None",
                      col == 2, "To Pain",
                      col == 3, "To Voice",
                      col == 4, "Spontaneous",
                      default = "Unknown")
  return(col_value)
}

# Assessment of the stimulus required for verbal response 
# (or written response if verbal response is impaired by intubation or tracheostomy), 
# assessed within 30 minutes or less of ED/hospital arrival.  (pg. 67)
verbal_resp_func <- function(col) {
  col_value <-  case.(col == 1, "None",
                      col == 2, "Incomprehensible Sounds",
                      col == 3, "Inappropriate Words",
                      col == 4, "Confused",
                      col == 5, "Oriented", 
                      default = "Unknown")
  return(col_value)
}

# Assessment of the stimulus necessary to elicit motor response, 
# assessed within 30 minutes or less of ED/hospital arrival. (pg. 68)
motor_resp_func <- function(col) {
  col_value <-  case.(col == 1, "None",
                      col == 2, "Extension",
                      col == 3, "Flexion",
                      col == 4, "Withdraws",
                      col == 5, "Localizes pain",
                      col == 6, "Obeys Command", 
                      default = "Unknown")
  return(col_value)
}


# The categories are based on HC4 standard categories (pg. 101)
insurance_func <- function(col) {
  col_value <-  case.(col == 1, "Medicare Indemnity",
                      col == 2, "Medicare Managed Care",
                      col == 3, "Medicaid Indemnity",
                      col == 4, "Medicaid Managed Care",
                      col == 5, "Commercial Insurer Indemnity",
                      col == 6, "Commercial Insurer Managed Care", 
                      col == 7, "Other Third-party", 
                      col == 8, "Self Pay", 
                      default = "Unknown")
  return(col_value)
}

# Record the status of the patient upon discharge from the hospital submitting this data  (pg. 77)
discharge_status_func <- function(col) {
  col_value <-  case.(col == 6, "Patient was dischared alive",
                      col == 7, "Patient died",
                      default = "Unknown")
  return(col_value)
}

# The destination of the patient on formal discharge (pg. 79)
discharge_destination_func <- function(col) {
  col_value <-  case.(col == 1, "Home",
                      col == 2, "Other Hospital",
                      col == 4, "Rehabilitation Center",
                      col == 5, "Skilled Nursing Facility",
                      col == 6, "Burn Center", 
                      col == 7, "Psychiatric Facility", 
                      col == 8, "Legal Authority", 
                      col == 9, "Drug or Alcohol Rehab ", 
                      col == 10, "Other Supervised Residential Facility", 
                      col == 11, "AMA", 
                      col == 12, "Homeless", 
                      col == 13, "Transitional Care Unit",
                      col == 14, "Pennsylvania Trauma Center", 
                      col == 15, "Out of State Trauma Center", 
                      col == 16, "Long Term Care Acute Care Center", 
                      col == 17, "Hospice", 
                      col == 18, "Foster Care", 
                      default = "Unknown")
  return(col_value)
}


# Custom age ranges
age_range_func <- function(col) {
  col_value <-  case.(col < 100, "90-100 yrs", 
                      col < 90, "80-90 yrs", 
                      col < 80, "70-80 yrs", 
                      col < 70, "60-70 yrs", 
                      col < 60, "50-60 yrs", 
                      col < 50, "40-50 yrs", 
                      col < 40, "30-40 yrs", 
                      col < 30, "20-30 yrs", 
                      col < 20, "15-20 yrs", 
                      col < 15, "10-15 yrs", 
                      col < 10, "5-10 yrs",
                      col < 5, "2-5 yrs",
                      col < 2, "1-2 years",
                      col < 1, "0-1 years",
                      col >= 100, "100+ yrs",
                      default = "Unknown")
  return(col_value)
}

# Custom age ranges
fall_height_func <- function(col) {
  col_value <-  case.(col = 0, "Fall on same level",  
                      col = 1, "Less than or equal to 1 ft", 
                      col = 2, "2 to 5 ft", 
                      col = 3, "6 to 10 ft", 
                      col = 4, "11 to 20 ft", 
                      col = 5, "21 to 30 ft", 
                      col = 6, "Greater than 30 ft", 
                      col = 'I', "Inappropriate - patient did not fall", 
                      col = 'U', "Unknown - no distance can be estimated", 
                      default = "Unknown")
  return(col_value)
}

# Location of procedure
proc_loc_func <- function(col) {
  col_value <-  case.(col == 1, "ED",
                      col == 2, "OR",
                      col == 3, "ICU",
                      col == 4, "Med/Surg Floor",
                      col == 5, "Step-down Unit", 
                      col == 6, "Radiology", 
                      col == 7, "Neclear Medicine", 
                      col == 8, "Burn Unit", 
                      col == 9, "PMR", 
                      col == 10, "Minor Surgery Unit", 
                      col == 13, "PACU", 
                      col == 14, "Postmortem" , 
                      col == 15, "EMS", 
                      col == 16, "Referring Facility", 
                      col == 17, "Special Procedure Unit",
                      col == 18, "Angoigraphy", 
                      default = "Unknown")
  return(col_value)
}



