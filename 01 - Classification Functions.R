

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
  col_value <- case.(col == 1 , 'Adams' ,
                     col == 2 , 'Allegheny' ,
                     col == 3 , 'Armstrong' ,
                     col == 4 , 'Beaver' ,
                     col == 5 , 'Bedford' ,
                     col == 6 , 'Berks' ,
                     col == 7 , 'Blair' ,
                     col == 8 , 'Bradford' ,
                     col == 9 , 'Bucks' ,
                     col == 10 , 'Butler' ,
                     col == 11 , 'Cambria' ,
                     col == 12 , 'Cameron' ,
                     col == 13 , 'Carbon' ,
                     col == 14 , 'Centre' ,
                     col == 15 , 'Chester' ,
                     col == 16 , 'Clarion' ,
                     col == 17 , 'Clearfield' ,
                     col == 18 , 'Clinton' ,
                     col == 19 , 'Columbia' ,
                     col == 20 , 'Crawford' ,
                     col == 21 , 'Cumberland' ,
                     col == 22 , 'Dauphin' ,
                     col == 23 , 'Delaware' ,
                     col == 24 , 'Elk' ,
                     col == 25 , 'Erie' ,
                     col == 26 , 'Fayette' ,
                     col == 27 , 'Forest' ,
                     col == 28 , 'Franklin' ,
                     col == 29 , 'Fulton' ,
                     col == 30 , 'Greene' ,
                     col == 31 , 'Huntingdon' ,
                     col == 32 , 'Indiana' ,
                     col == 33 , 'Jefferson' ,
                     col == 34 , 'Juniata' ,
                     col == 35 , 'Lackawanna' ,
                     col == 36 , 'Lancaster' ,
                     col == 37 , 'Lawrence' ,
                     col == 38 , 'Lebanon' ,
                     col == 39 , 'Lehigh' ,
                     col == 40 , 'Luzerne' ,
                     col == 41 , 'Lycoming' ,
                     col == 42 , 'McKean' ,
                     col == 43 , 'Mercer' ,
                     col == 44 , 'Mifflin' ,
                     col == 45 , 'Monroe' ,
                     col == 46 , 'Montgomery' ,
                     col == 47 , 'Montour' ,
                     col == 48 , 'Northhampton' ,
                     col == 49 , 'Northumberland' ,
                     col == 50 , 'Perry' ,
                     col == 51 , 'Philadelphia' ,
                     col == 52 , 'Pike' ,
                     col == 53 , 'Potter' ,
                     col == 54 , 'Schuylkill' ,
                     col == 55 , 'Snyder' ,
                     col == 56 , 'Somerset' ,
                     col == 57 , 'Sullivan' ,
                     col == 58 , 'Susquehanna' ,
                     col == 59 , 'Tioga' ,
                     col == 60 , 'Union' ,
                     col == 61 , 'Venango' ,
                     col == 62 , 'Warren' ,
                     col == 63 , 'Washington' ,
                     col == 64 , 'Wayne' ,
                     col == 65 , 'Westmoreland' ,
                     col == 66 , 'Wyoming' ,
                     col == 67 , 'York' ,
                     col == 68 , 'Delaware' ,
                     col == 69 , 'Maryland' ,
                     col == 70 , 'New Jersey' ,
                     col == 71 , 'New York' ,
                     col == 72 , 'Ohio' ,
                     col == 73 , 'West Virginia' ,
                     col == 74 , 'Other State' ,
                     col == 75 , 'Virginia' ,
                     col == 79 , 'Other Country' ,
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
  col_value <-  case.(col == 0 , 'Fall on Same Level' ,
                      col == 1 , '<= 1 ft' ,
                      col == 2 , '2 - 5 ft' ,
                      col == 3 , '6 - 10 ft' ,
                      col == 4 , '11 - 20 ft' ,
                      col == 5 , '21 - 30 ft' ,
                      col == 6 , '> 30 ft' ,
                      col == 'I', "Inappropriate - patient did not fall", 
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


# Occurances and Complications (pg. 122-125)
complications_func <- function(col) {
  col_value <-  case.(col == 1 , 'None' ,
                      col == 10 , 'Burn Graft Loss (of any percentage)' ,
                      col == 11 , 'Burn Wound Infection Post Excision' ,
                      col == 12 , 'Burn Wound Sepsis' ,
                      col == 13 , 'Burn Wound Cellulitis' ,
                      col == 14 , 'Delay in Donor Site Healing' ,
                      col == 15 , 'Hypovolemia' ,
                      col == 20 , 'Adult Respiratory Distress Syndrome (ARDS)' ,
                      col == 201 , 'Drug or Alcohol Withdrawal Syndrome' ,
                      col == 202 , 'Unplanned Intubation' ,
                      col == 203 , 'Unplanned Return to the OR' ,
                      col == 204 , 'Unplanned Admission to the ICU' ,
                      col == 205 , 'Stroke/CVA' ,
                      col == 206 , 'Cardiac Arrest with Resuscitative Efforts by Healthcare Provider' ,
                      col == 21 , 'Acute Respiratory Failure' ,
                      col == 22 , 'Aspiration/Aspiration Pneumonia' ,
                      col == 23 , 'Atelectasis' ,
                      col == 24 , 'Fat Embolus Syndrome' ,
                      col == 25 , 'Hemothorax' ,
                      col == 26 , 'Pneumonia' ,
                      col == 27 , 'Iatrogenic Pneumothorax' ,
                      col == 28 , 'Pulmonary Embolus (PE)' ,
                      col == 29 , 'Pulmonary Arrest' ,
                      col == 30 , 'Acute Arterial Occlusion (not present on admission)' ,
                      col == 31 , 'Cardiac Arrest' ,
                      col == 32 , 'Extremity Compartment Syndrome (not present on admission)' ,
                      col == 33 , 'Deep Vein Thrombosis (DVT)' ,
                      col == 34 , 'Major Dysrhythmia' ,
                      col == 35 , 'Myocardial Infarction (MI)' ,
                      col == 36 , 'Acute Arterial Embolus' ,
                      col == 37 , 'Acute Arterial Occlusion' ,
                      col == 38 , 'Acute Arterial Thrombosis' ,
                      col == 39 , 'Arrythmia' ,
                      col == 40 , 'Blood Transfusion Reaction' ,
                      col == 41 , 'Coagulopathy' ,
                      col == 42 , 'Congestive Heart Failure' ,
                      col == 43 , 'Acute Tracheobronchitis' ,
                      col == 44 , 'Pleural Effusion' ,
                      col == 45 , 'Pulmonary Edema' ,
                      col == 46 , 'Hypothermia' ,
                      col == 47 , 'Post-Operative Hemorrhage' ,
                      col == 48 , 'Cardiopulmonary Arrest (unexpected, not resulting in death)' ,
                      col == 49 , 'Adverse Drug Reaction' ,
                      col == 50 , 'Acute Kidney Injury' ,
                      col == 51 , 'Syndrome Inappropriate Antidiuretic Hormone (SIADH)' ,
                      col == 60 , 'Cholecystitis' ,
                      col == 61 , 'Hepatitis' ,
                      col == 62 , 'Hyperbilirubinemia' ,
                      col == 63 , 'Liver Failure' ,
                      col == 64 , 'CNS Infection' ,
                      col == 65 , 'Dehiscence/Evisceration' ,
                      col == 66 , 'Progression of Original Neurologic Insult' ,
                      col == 68 , 'Delirium Tremens (DTs)' ,
                      col == 69 , 'Unrecognized Mainstem Bronchus Intubation' ,
                      col == 70 , 'Empyema' ,
                      col == 71 , 'Fungal Sepsis' ,
                      col == 72 , 'Intra-abdominal Abscess' ,
                      col == 73 , 'Meningitis' ,
                      col == 74 , 'Osteomyelitis' ,
                      col == 75 , 'Other Abscess' ,
                      col == 76 , 'Sepsis' ,
                      col == 77 , 'Septicemia' ,
                      col == 78 , 'Acute Sinusitis' ,
                      col == 79 , 'Soft Tissue Infection' ,
                      col == 80 , 'Esophageal Intubation (inhouse-only)' ,
                      col == 81 , 'Fistula, Enterocutaneous' ,
                      col == 82 , 'Fistula, Enteroenteric' ,
                      col == 83 , 'GI Bleeding' ,
                      col == 84 , 'Pancreatitis' ,
                      col == 85 , 'Pseudomembranous Colitis/C. Difficle' ,
                      col == 86 , 'Small Bowel Obstruction (SBO) (excluding ileus)' ,
                      col == 87 , 'Anoxic Encephalopathy' ,
                      col == 88 , 'Diabetes Insipidus' ,
                      col == 89 , 'Cerebral Infarct/Stroke' ,
                      col == 90 , 'Other Neurologic Sequelae' ,
                      col == 91 , 'Iatrogenic Organ, Nerve, Vessel' ,
                      col == 92 , 'Dehiscence' ,
                      col == 93 , 'Non-Traumatic Evisceration' ,
                      col == 94 , 'Decubitus' ,
                      col == 95 , 'Other not listed' ,
                      col == 96 , 'Seizures' ,
                      col == 97 , 'Urinary Tract Infection (UTI) (not present on admission)' ,
                      col == 98 , 'Ventriculitis' ,
                      col == 99 , 'Wound Infection (traumatic or incisional)')
  return(col_value)
}


# Post Graduate Level for Resident - a way to classify experience (pg. 60)
pgy_func <- function(col) {
  col_value <-  case.(col == 1 ,  'Year 1' ,
                      col == 2 ,  'Year 2' ,
                      col == 3 ,  'Year 3' ,
                      col == 4 ,  'Year 4' ,
                      col == 5 ,  'Year 5' ,
                      col == 6 ,  'Year 6' ,
                      col == 7 ,  'Year 7' ,
                      col == 8 ,  'Year 8' ,
                      col == 9 ,  'Year 9' ,
                      col == 'F', 'Fellow' ,
                      col == 'U', 'Not Documented', 
                      default =   'Not Documented' )
  return(col_value)
}

# Pre-existing Condition
preexisting_condition_func <- function(col) {
  col_value <-  case.(col == '0.00',    'None',
                      col == 'A.01',    'History of Cardiac Surgery',
                      col == 'A.02',    'Coronary Artery Disease',
                      col == 'A.03',    'Congestive Heart Failure',
                      col == 'A.04',    'Cor Pulmonale',
                      col == 'A.05',    'Myocardial Infarction',
                      col == 'A.06',    'Hypertension',
                      col == 'A.07',    'Congenital Cardiac Disease',
                      col == 'B.03',    'Diabetes Mellitus',
                      col == 'C.01',    'Peptic Ulcer Disease',
                      col == 'C.02',    'Gastric or Esophageal Varices',
                      col == 'C.03',    'Pancreatitis',
                      col == 'C.04',    'Inflammatory Bowel Disease',
                      col == 'C.05',    'Bariatric Surgery',
                      col == 'D.01',    'Acquired Coagulopathy',
                      col == 'D.02',    'Reversible Anticoagulant Therapy',
                      col == 'D.04',    'Pre-existing Anemia',
                      col == 'D.05',    'Anti-platelet Agents',
                      col == 'D.07',    'Non-reversible Anticoagulant Therapy',
                      col == 'D.08',    'Other Bleeding Disorder',
                      col == 'E.00',    'History of Psychiatric Disorders',
                      col == 'E.01',    'Attention Deficit Disorder',
                      col == 'E.02',    'Mental Retardation',
                      col == 'F.01',    'HIV/AIDS',
                      col == 'F.02',    'Routine Steroid Therapy',
                      col == 'F.03',    'Transplants',
                      col == 'F.04',    'Active Chemotherapy',
                      col == 'G.01',    'Bilirubin > 2 mg% (On Admission)',
                      col == 'G.02',    'Documented History of Cirrhosis',
                      col == 'H.01',    'Undergoing Current Therapy',
                      col == 'H.02',    'Concurrent or Existence of Metastasis',
                      col == 'H.03',    'History of Pediatric Malignancy',
                      col == 'I.01',    'Arthritis',
                      col == 'I.02',    'Systemic Lupus Erythematosus',
                      col == 'I.03',    'Osteogenisis (OI)',
                      col == 'J.01',    'Spinal Cord Injury',
                      col == 'J.02',    'Multiple Sclerosis',
                      col == 'J.03',    'Alzheimers Disease',
                      col == 'J.04',    'Seizures',
                      col == 'J.05',    'Chronic Demyelinating Disease',
                      col == 'J.06',    'Chronic Dementia',
                      col == 'J.07',    'Organic Brain Syndrome',
                      col == 'J.08',    'Parkinsons Disease',
                      col == 'J.09',    'CVA',
                      col == 'J.10',    'Autism Spectrum',
                      col == 'J.11',    'Cerebral Palsy (CP)',
                      col == 'K.00',    'Obesity',
                      col == 'L.05',    'Respiratory Disease',
                      col == 'M.01',    'Serum Creatinine > 2 mg% (On Admission)',
                      col == 'M.02',    'Dialysis (Excludes Transplant Patients)',
                      col == 'N.01',    'Drug Use Disorder',
                      col == 'N.02',    'Chronic Ongoing Alcohol Abuse',
                      col == 'P.00',    'Pregnancy',
                      col == 'Q.00',    'Any Previous History of Admission for Trauma or Burn',
                      col == 'Q.01',    'Previous History of Head Trauma',
                      col == 'R.01',    'Thyroid Disease',
                      col == 'S.01',    'Ascites within 30 Days',
                      col == 'S.02',    'Current Smoker',
                      col == 'S.03',    'Advanced Directive Limiting Care',
                      col == 'S.04',    'Functionally Dependent Health Status',
                      col == 'S.05',    'History of Angina within 30 Days',
                      col == 'S.06',    'History of PVD',
                      col == 'S.07',    'Prematurity',
                      col == 'S.08',    'Pre-hospital Cardiac Arrest',
                      col == 'T.00',    'Congenital Disorder',
                      default =          'Not Documented' )
  return(col_value)
}


# Protective Equipment
protective_equipment_func <- function(col) {
  col_value <-  case.(col == 0,    'None',
                      col == 1,    'Seatbelt',
                      col == 2,    'Car Seat',
                      col == 3,    'Airbag (Deployed)',
                      col == 4,    'Helmet',
                      col == 5,    'Seatbelt & Airbag (Deployed)',
                      col == 6,    'Sports Equipment',
                      col == 7,    'Industrial Equipment',
                      col == 8,    'Booster Seat', 
                      default = NA)
  return(col_value)
}

# Service or Specality - not the same as surgical speciality
service_func <- function(col) {
  col_value <-  case.(col == 0,    'None',
                      col == 1,    'Trauma',
                      col == 2,    'Neurosurgery',
                      col == 3,    'Orthopedics',
                      col == 4,    'Thoracic Surgery',
                      col == 5,    'Vascular Surgery',
                      col == 6,    'Pediatrics',
                      col == 7,    'Oromaxillo Facial Service',
                      col == 8,    'OB/GYN',
                      col == 9,    'Burn Services',
                      col == 10,    'Cardiology',
                      col == 11,    'Cardiothoracic Surgery',
                      col == 12,    'Drug/Alcohol Counselor',
                      col == 13,    'ENT',
                      col == 14,    'Family Medicine',
                      col == 15,    'General Surgery',
                      col == 16,    'Infectious Disease',
                      col == 17,    'Internal Medicine',
                      col == 18,    'Nephrology',
                      col == 19,    'Neurology',
                      col == 20,    'Nutrition',
                      col == 21,    'Occupational Therapy',
                      col == 22,    'Ophthalmology',
                      col == 23,    'Oral Surgery',
                      col == 24,    'Physiatry',
                      col == 25,    'Physical Therapy',
                      col == 26,    'Plastic Surgery',
                      col == 27,    'Psychiatry',
                      col == 28,    'Pulmonary',
                      col == 29,    'Social Services',
                      col == 30,    'Speech Therapy',
                      col == 31,    'Urology',
                      col == 32,    'Case Management',
                      col == 33,    'Palliative Care',
                      col == 34,    'Pastoral Care',
                      col == 35,    'Geriatrics/Gerontology',
                      col == 99,    'Other'
                      default = NA)
  return(col_value)
}
