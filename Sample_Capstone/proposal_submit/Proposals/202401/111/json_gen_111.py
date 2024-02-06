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
            """111""",
        # -----------------------------------------------------------------------------------------------------------------------
        "Year":
            """2024""",
        # -----------------------------------------------------------------------------------------------------------------------
        "Semester":
            """Spring""",
        # -----------------------------------------------------------------------------------------------------------------------
        "project_name":
            "",
        # -----------------------------------------------------------------------------------------------------------------------
        "Objective":
            """ 
            In the state of Massachusetts, there continues to be a racial divide in nearly every aspect of daily life. Through my project,
            I'm focusing specifically on public schools and real estate, and analyzing how these two issues are intrinsically intertwined.
            Using primarily state data and research (by analyzing local journalism findings, university reports, and possibly my own interviews etc)
            I hope to produce a story-scrolling project that not only tells the story of the reality of racial segregation in the state, 
            but also how it can be addressed in the future through more comprehensive policy changes. 
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Dataset":
            """
            I will be using data sets in order to address different questions to undertake this project. The following topics 
            will be researched using the corresponding dataset:
            - School segregation: Mass. Dept. of Education; ProPublica
            - Real Estate / Redlining: Mass.gov 
            - Poverty & Unemployment: US Census; Bureau of Labor Statistics (BLS)
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Rationale":
            """
            The goal is to yield a data journalism project that details the underlying reasons how Massachuetts has maintains systemic barriers 
            in its institutions that result in a deep racial divide in access, especially in two major areas of life:
            1. Education 
            2. Real Estate
            Often, education reform and barriers to affordable housing end up discussed as separate issues. However, they are fundamentaly
            intertwined. For example, while busing and other methods of desegregating MA public schools does yield better outcomes for students,
            according to a recent study from Tufts, being able to actually integregate wealthier, often majority white towns, through housing 
            reform is a solution that is often left out of conversation. 
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
           I'm not sure if I'll conduct many of my own interviews, however, in order to supplement my data investigation
           and visualizations, I'll include supplementary quotes at least from leading newspapers from the Boston Globe to 
           supplement my research, and either affirm or debunk my hypothesis that housing reform is the avenue towards real
           education reform, due to its potential to aid in desegregation and increase diversity primarily. 
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
