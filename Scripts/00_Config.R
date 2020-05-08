## PROJECT:  USG - Foreign Assistance
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Spatial distribution of funding by sector
## DATE: 2020-05-08


# FUNCTION ----------------------------------------------------------------


folder_setup <- function(folder_list = list("Data", "Images", "Scripts", 
                                            "Dataout", "GIS", "Documents", "Graphics", "markdown")) {
    if(!is.list(folder_list))
        stop("Please provide a list of directories to create for the project.")
    print("The following directories will be created:")
    print(glue::glue(crayon::green('{folder_list}')))
    purrr::map(folder_list, ~dir.create(.))
    
}

# Set global shortucts ----------------------------------------------------

folder_setup()
