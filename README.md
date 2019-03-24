## Sankey Diagram Visualization for Severe Sepsis Pathways integrating Alerts with Treatment Bundles

#### Introduction 

Sepsis is an important public health problem in the United States and is the leading cause of death among hospitalized patients. Different hospitals adapt the recommended guidelines for sepsis management differently to their standards of care. To further improve the care process, we need to better understand the current clinical pathways in terms of adherence to the guidelines. However, we should also keep in mind that sepsis remains difficult to diagnose, which considerably complicates the clinical pathways. Electronic Medical Records (EMRs) contain adequate timestamped data for identifying the critical events describing how severe sepsis evolves over time with or without timely treatments. In this abstract, we introduced a process mining technique to summarize and visualize the interactive behaviors of sepsis alerts and treatments in temporal order inspired by [1].

[1] https://www.sciencedirect.com/science/article/pii/S1532046415001306 

Click here to see the [static view] of the sankey diagram     
Click here to see the [interactive view] of the sankey diagram

[static view]: http://rpubs.com/sxinger/static_sankey
[interactive view]: https://sxinger.shinyapps.io/interactive_sankey/

***

##### Study Cohort Definition:
**Inclusion Criteria**    

* Age above 18 at ED Admissions   
* Suspected Infection (SI) defined as:    
* --Blood culture ordered at most 4 hours before antibiotics; or    
* --Antibiotics administered at most 4 hours before blood culture   


**Exclusion Criteria**    

* Suspected Infection occurred beyond 13 hours since triage   


**Final Study Cohort**    

* 15,616 eligible encounters of Suspected Infection   

**Primary Outcome**   

* Severe Sepsis Onset, defined as the last occurrence of:   
* -- Suspected Infection    
* -- At least 2 SIRS (Systemic Inflammatory Response Syndrome)    
* -- First site of acute organ dysfunction defined by the first presence of an abnormal laboratory value:   
*    -- Altered Mental Status (AMS): Glasgow Coma Scale<15 or Level of Consciousness at states other than “Awareness”   
*    -- Hypotension: SBP < 70mmHG or MAP < 90mmHG   
*    -- Liver Failure (LF): a bilirubin increase of as least 3 mg/dL from patient’s baseline or bilirubin>4 mg/dL
*    -- Hypoxemia: SpO2<90%, or PaO2<70 mm Hg, or PaO2/FiO2<300     
*    -- Kidney Failure (KF): a serum creatinine increase of at least 0.5 mg/dL from patient’s baseline or serum creatinine>2.0 mg/dL
*    -- Abnormal Coagulation (AC): platelet counts<100,000/mm3 or INR>1.5 or PTT>40 or d-dimer>200 Increased Lactate (IL): lactate>2.0 mmol/L

***

##### Determine Sepsis Alerts Timestamps

* Onset time of suspected infection (the earlier occurrence of blood culture order and antibiotic administration)   
* Onset times of SIRS (first abnormal temperature, heart rate, respiratory rate, and WBC)   
* Onset times of acute organ dysfunctions (first presence of an abnormal laboratory values)   

##### Determine 3-hour bundle Timestamps    

* Result time for initial lactate   
* Order time for first blood culture    
* Administration time for first broad-spectrum antibiotics    
* Initiation time of IV fluid bolus   
* Completion time IV fluid bolus at 30 ml/kg    

##### Event Trace     
* The sequence of timestamped entries comprising both sepsis alerts and treatment components    
* -- e.g. `SIRS2@1--BLOOD_C@2--LAC1@3` means 2 SIRS occurred first, followed by blood culture and initial lactate, where `@X` refers to relative temporal order    
* -- 2,538 distinct non-trivial event traces were identified 

