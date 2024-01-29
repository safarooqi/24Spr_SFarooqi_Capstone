#%%
import json
import os
import shutil

#%%

def save_to_json(data, output_file_path):
    with open(output_file_path, 'w') as output_file:
        json.dump(data, output_file, indent=2)

semester2code = { "sp":"01", "spr":"01", "spring":"01", "su":"02", "sum":"02", "summer":"02", "fa":"03", "fall":"03"}
thisfilename = os.path.basename(__file__) # should match _ver for version, ideally 3-digit string starting as "000", up to "999"

data_to_save = \
    {
        # -----------------------------------------------------------------------------------------------------------------------
        "Version":
            """000""",
        # -----------------------------------------------------------------------------------------------------------------------
        "Year":
            """2024""",
        # -----------------------------------------------------------------------------------------------------------------------
        "Semester":
            """Spring""",
        # -----------------------------------------------------------------------------------------------------------------------
        "project_name":
            """'The Most Racist City in America'—How Boston, and beyond, gained its notorious reputation""",
        # -----------------------------------------------------------------------------------------------------------------------
        "Objective":
            """ 
            Urban renewal and redlining was a phenomenon throughout the 20th century that led to the clearing of minority and immigrant  
            communities for highway projects and new developments through the 1950s and 60s. So, what has been the impact? How did redlining  
            maps ultimately worsen segregation in Massachusetts, so much so, there continues to be a racial divide in nearly every aspect  
            of daily life? From public schools, to health care access, to real estate, and poverty—nearly every facet of life in MA is  
            dictated by race. Using state data, interviews, and historical research, I hope to produce a story-scrolling project 
            that tells the story of racial segregation in the state, specifically focusing how urban renewal and redlining has shaped 
            its evolution through the 20th and 21st centuries. 
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Dataset":
            """
            I will be using several data sets in order to address different questions to undertake this project. The following topics 
            will be researched using the corresponding dataset:
            - School segregation: Mass. Dept. of Education; ProPublica
            - COVID-19 impact disparities: CDC, Mass.gov
            - Real Estate / Redlining: Mass.gov 
            - Poverty & Unemployment: US Census; Bureau of Labor Statistics (BLS)
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Rationale":
            """
            The goal is to yield a data journalism project that details the underlying reasons how Boston, and the entire state of MA
            has created systemic barriers in its institutions that result in a deep racial divide in access to at least four major areas of life:
            1. Education
            2. Employment 
            3. Real Estate
            4. Health Care
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Approach":
            """
            In order to ultimately yield a story-scrolling data visualization project, I will do the following:

            1. Import all data to an RStudio markdown file
            2. Depending on the data, first undertake a statistical analysis 
            3. Then, create maps 
            4. Utilize proper storyscrolling package in R
            5. Incorporate research & interviews between each visualization & subject   
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Timeline":
            """
            This a rough time line for this project (12 weeks total):  

            - (2 Weeks) Initial research, data importing, & cleaning
            - (2 Weeks) Research, and potentially interviewing 
            - (3 Weeks) Building visualizations  
            - (2 Weeks) Writing story based on data research and interviews
            - (1 Weeks) Final Presentation  
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Expected Number Students":
            """
            I'll be working on this project alone.
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Possible Issues":
            """
            I may be trying to take on too many subjects in one project. May potentially need to narrow my focus. 
            May also need to reassess my data sources.
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Proposed by": "Samiha Farooqi",
        "Proposed by email": "ssfarooqi@gwu.edu",
        "instructor": "Edwin Lo",
        "instructor_email": "edwinlo@email.gwu.edu",
        "github_repo": "https://github.com/safarooqi/24Spr_SFarooqi_Capstone",
        # -----------------------------------------------------------------------------------------------------------------------
    }
os.makedirs(
    os.getcwd() + f'{os.sep}Proposals{os.sep}{data_to_save["Year"]}{semester2code[data_to_save["Semester"].lower()]}{os.sep}{data_to_save["Version"]}',
    exist_ok=True)
output_file_path = os.getcwd() + f'{os.sep}Proposals{os.sep}{data_to_save["Year"]}{semester2code[data_to_save["Semester"].lower()]}{os.sep}{data_to_save["Version"]}{os.sep}'
save_to_json(data_to_save, output_file_path + "input.json")
shutil.copy(thisfilename, output_file_path)
print(f"Data saved to {output_file_path}")
