# PetfindeR STA-141b-Project

### The primary goal of this project is to help users access this database through an application, find information that may be beneficial in their pet adoption process, and sort that information in a logical and clear way using data tables and charts. This application will allow users to search for and display pet listings based on various characteristics based on those unique to the pet and output relevant to their search for that given pet. Users may also search for and display animal welfare organizations based on organization name, ID, and location.
Depending on the needs of the user, they may display a random assortment of available pets for adoption, display pets in various categories, pick pets to choose from based on specific criteria, and view visual representations on pet availability. For this project, our focus is to display the pets available in California, narrowing our database to just one state and the animal welfare organizations in that one state. 

This project will be using the Petfinder Application Programming Interface (API). This database allows us to use data on hundreds of thousands of animals ready for adoption over ten thousand animal welfare organizations. We will use this API to build a Shiny application to allow users, who do not necessarily have any statistics background, to interact with the data and view it via some graphical visualizations, in addition to searching for animals based on different preferences and characteristics. 

## Background of Database
The Petfinder API (Application Programming Interface) provides access to a database of hundreds of thousands of pets ready for adoption at over ten thousand animal welfare organizations. The database is composed of numerous query parameters . These parameters include, but are not limited to, string parameters such as type of animal, breed of animal, coat, size, age, color, gender, name, organization, and status. In addition to the string parameters representing data of the string type, the Petfinder database also includes data of the boolean type such as animals “good_with_children” or “good_with_dogs.” The database also sorts the results through location with a parameter of the  integer type.

## Retrieving the Data
In order to retrieve the data, we first had to get authenticated. The Petfinder API uses OAuth to ensure secure authentication.
To get started, we created a Petfinder account. Once creating the count, we were then able to receive a Petfinder API Key (also called Client ID) and Secret.
Since this is a RESTful API, it uses reliable URLs to retrieve resources and, in the event of an error, will return meaningful HTTP response codes. This enables the use of GET, POST, and HTTP authentication, which standard HTTP clients understand. The API then supports cross-origin resource sharing, allowing us to use it securely from a client-side web application. We then used the API by sending requests with a specific structure to our servers. In order to maintain security, the API uses access tokens for API requests. 
After receiving access to the database, we were also able to access the data through installing the package, “Petfinder” and calling the “Petfinder” library on R. Using various functions in R we were then able to organize the data and narrow it down to California organizations and specific types of animals we are interested in learning the status of for adoption. Since the database and library were very large, we converted the database into a CSV file with relevant information for our application such as organization ID, URL to adoption website, age, gender, breeds and more. 

Another option, is to download petfindeR package in R, by `library(petfindeR)`. Check out the package vignette here: https://cran.r-project.org/web/packages/PetfindeR/vignettes/Introduction_to_PetfindeR.html 

## Userguide 
	How to Use Petfinder ©:
Designed for new and experienced pet owners, looking to adopt an animal in California. 
Once navigating to the app and accessing the Petfinder application on shinyapps.io, users may narrow their search for a given animal by selecting desired features in the series of drop down menus. The feature includes drop down menus for Animal Type, Breed, Size, Age (baby, young, adult, senior), and Gender. On the right, the user also has the option to input any desired characteristics into the search bar. 

The application then outputs data that matches the specified and preferred characteristics users desire in the animal they are looking to adopt. In addition to providing data that matches the user’s preferences, the application also provides available data for preferences that were not specified in the drop down menu as well as each animal’s identification number, the organization id, website URL for the adoption agency, and pet’s name if the animal has one. 

Another feature of the application is a more visual representation of available animals to adopt in California distinguished by Animal Type. The application outputs a Bar Chart coded to display the frequency of a certain type of animal by the animal size (extra large, large, medium, small) and age category (adult, baby, senior, young). 

