# citizens (simple class of name, age, education, ...)
# buildings (more complex class: has to house either bussinesses, people (or both), services, capital (success of a building), maintenance level, ...)

# city (final table of all buildings (for complexity infrastructure is not accounted for (likely)))
# ?? citizens as its own table, then pointed to by other objects (for sharing citizens between buildings: housing / work / ...)


# simple command line (ASCII) interface
# progress in console-command driven ticks (year quarters??)
# graphs and other options part of the command-line interface, ...


# Supply/Demand model
# - citizens increase specific demand (1 person = 1 unit)
# - industry/retail supply specifics (1 building = X units)
# - ?? new bussinesses from demand?

# Import/Export
# - Supply/demand is for citizens (commercial supplies, citizens demand)
# - Import/Export is for city (commercial imports, industry exports (trading for city capital))


##### OLD #####

# # citizen creator
# make_citizen <- function(name, age) {
#   structure(
#     list(name = name, age = age),
#     class = "citizen"
#   )
# }
#
#
# # citizen_1 <- make_citizen(name = "Jay", age = 1)
#
# # building creator
# # make_building <- function(x = 0, y = 0) { # add size / floors / both
# #   structure(
# #     list(x = x, y = y),
# #     class = "building"
# #   )
# # }
#
#
# city <- data.frame()
#
#
# # from point in space finds closest empty plot coords (simple)
# nearest_empty_plot <- function(x_base, y_base) {
#
#   # check base coords
#   if(!any(city$x == x_base & city$y == y_base)){
#     return(c(x_base, y_base))
#   }
#
#   x_offset = 1
#   y_offset = 1
#
#   # loop
#   while(TRUE) {
#
#     # check fresh surroundings
#     for (y in (y_base - y_offset):(y_base + y_offset)){
#       for (x in (x_base - x_offset):(x_base + x_offset)){
#         if(!any(city$x == x & city$y == y)){
#           return(c(x, y))
#         }
#       }
#     }
#
#     # increment surroundings
#     y_offset = y_offset + 1
#     x_offset = x_offset + 1
#   }
# }
#
# populate_buildings <- function(amount = 1) {
#   for (id in (nrow(city))+1:(nrow(city)+amount)) {
#
#     # find empty space
#     coords <- nearest_empty_plot(2, 2)
#     x = coords[1]
#     y = coords[2]
#
#     # build
#     cat("built: (x: ", x, ", y: ", y, ")\n")
#
#     city[id, "x"] <<- x # <<- is a global assignment operator
#     city[id, "y"] <<- y
#   }
# }
#
#
# populate_buildings(9)
#


##### SETTINGS #####

COMPLEX_LOG = FALSE
INIT_POPULTAION = 100
INIT_CITYSIZE = 20

commercial_preset <- c("food" = 100, "clothing" = 200, "household_goods" = 100, "pharmaceuticals" = 200, "electronics" = 200, "furniture" = 400)
industrial_preset <- c("food" = 20, "clothing" = 100, "household_goods" = 100, "pharmaceuticals" = 400, "electronics" = 100, "furniture" = 200)



##### CREATORS #####

# building creator
make_building <- function(type, resident_limit = 0, residents = list(), product = NULL, production = 0, positions = 0) { # add size / floors / both; maintainance, capital, ...
  structure(
    list(type = type, resident_limit = resident_limit, residents = residents, product = product, production = production, positions = positions),
    class = "building"
  )
}

# citizen creator
make_citizen <- function(name, age) { # add capital, working place, ...
  structure(
    list(name = name, age = age),
    class = "citizen"
  )
}



##### CODE BLOCKS #####

# from point in space finds closest empty plot coords (simple)
nearest_empty_cell <- function(x_base, y_base) {
  
  # check base coords
  
  city[[1, 1]] = 0
  
  if(is.null(city[[x_base, y_base]])){
    return(c(x_base, y_base))
  }
  
  x_offset = 1
  y_offset = 1
  
  # loop
  while(TRUE) {
    
    # check fresh surroundings
    for (y in (y_base - y_offset):(y_base + y_offset)){
      for (x in (x_base - x_offset):(x_base + x_offset)){
        if(is.null(city[[x, y]])){
          return(c(x, y))
        }
      }
    }
    
    # increment surroundings
    y_offset = y_offset + 1
    x_offset = x_offset + 1
  }
}

# populate_buildings <- function(x_center, y_center, type, amount = 1, product = NULL, production = 0, positions = 0) {
#   for (number in 1:amount) {
#
#     # find empty space
#     coords <- nearest_empty_cell(x_center, y_center)
#     x = coords[1]
#     y = coords[2]
#
#     # build
#     cat("built: (type: ", type, ", x: ", x, ", y: ", y, ")\n")
#
#     city[[x, y]] <<- make_building(type, list(), product, production, positions)
#   }
# }

place_building <- function(x, y, type, resident_limit = 0, product = NULL, production = 0, positions = 0) {
  # find nearest empty space
  coords <- nearest_empty_cell(x, y)
  x = coords[1]
  y = coords[2]
  
  # build
  if(COMPLEX_LOG)
    cat("built: (type: ", type, ", goods: ", product, ", x: ", x, ", y: ", y, ")\n")
  
  city[[x, y]] <<- make_building(type, resident_limit, list(), product, production, positions)
}

# City creation
new_city <- function(size) {
  DataSet <<- data.frame(
    Tick = numeric(), 
    Population = numeric(), 
    Buildings = numeric(),
    Residential_Buildings = numeric(),
    Commercial_Buildings = numeric(),
    Industrial_Buildings = numeric(),
    Births = numeric(), 
    Age_Deaths = numeric(), 
    Homeless_Deaths = numeric())
  tick_num <<- 0
  
  city_size <<- size
  city_center <<- as.integer(city_size/2)
  city <<- matrix(list(), nrow=city_size, ncol=city_size)
  city[[city_center, city_center]] <<- make_building("Town_hall")
  
  homeless <<- c()
  for(person in 1:INIT_POPULTAION){
    homeless <<- c(homeless, list(make_citizen(names[[(sample(1:length(names), 1))]], sample(18:50, 1))))
  }
  
  birth_rate_pool <<- 0
}

# City progression
progress <- function(ticks) {
  for (tick in 1:ticks){
    # populate_buildings(city_center, city_center)
    
    # calculate supply/demand
    # progress buildings
    # recalculate supply/demand
    # progress people ??
    # recalculate supply/demand ??
    
    ##### set up supply/demand and import/export lists #####
    
    meta_supply <<- c("housing" = 0, "positions" = 0)
    supply <<- commercial_preset * 0
    
    meta_demand <<- c("housing" = 0, "positions" = 0)
    demand <<- commercial_preset * 0
    
    import <<- industrial_preset * 0
    export <<- industrial_preset * 0
    
    
    ##### calculate demand ####
    # meta_demand <<- c("housing" = 0, "positions" = 0)
    # # demand <<- c("food" = 0)
    # demand <- commercial_preset * 0
    
    
    citizens <<- c()
    citizens_table <<- data.frame(matrix(ncol = 2, nrow = 0)) # possible replacement for old citizens list (DEBUG DEBUG DEBUG)
    colnames(citizens_table) <<- c("Name", "Age")
    
    for(building in city){
      if(!is.null(building)){
        #####
        # cat(building$type)
        # switch(
        #   building$type,
        #
        #   "town_hall" = {
        #     cat("cat")
        #   }
        #
        #   "residential" = {
        #     building$residents
        #   }
        # )
        
        # demand["housing"] <<- demand["housing"] + length(building$residents)
        # demand["food"] <<- demand["food"] + length(building$residents)
        # demand["positions"] <<- demand["positions"] + length(building$residents)
        #####
        # citizens <- citizens + length(building$residents)
        citizens <<- c(citizens, building$residents)
        
        
        for (resident in building$residents)
        {
          citizens_table[nrow(citizens_table)+1,] <<- list(Name = resident["name"], Age = resident["age"])
          # rbind(citizens_table, list(Name=resident["Name"], Age=resident["Age"]))
        }
      }
    }
    # citizens <- citizens + length(homeless)
    citizens <<- c(citizens, homeless)

    
    
    ##### calculate supply/demands and import/export #####
    # demands
    meta_demand["housing"] <<- length(citizens)
    meta_demand["positions"] <<- length(citizens)
    
    # demand["food"] <<- length(citizens)
    demand <<- demand + length(citizens)
    
    # supply
    # meta_supply <<- c("housing" = 0, "positions" = 0)

    # supply <<- c("food" = 0)
    
    
    
    # import <<- c("food" = 0)
    # export <<- c("food" = 0)
    
    for(building in city){
      if(!is.null(building)){
        if(building$type == "residential"){
          meta_supply["housing"] <<- meta_supply["housing"] + 10
          
        } else if(building$type == "commercial"){
          supply[building$product] <<- supply[building$product] + building$production
          import[building$product] <<- import[building$product] + building$production
          
        } else if(building$type == "industrial"){
          export[building$product] <<- export[building$product] + building$production
        }
      }
    }
    
    
    
    ##### expand #####
    meta_needs <<- meta_demand - meta_supply
    needs <<- demand - supply
    trade <<- export - import
    
    # needs
    # while(meta_needs["housing"] > 0){
    #   place_building(city_center, city_center, "residential", 10)
    #   meta_needs["housing"] <<- meta_needs["housing"]-10
    # }
    
    if(meta_needs["housing"] > 0){
      for(i in as.integer(meta_needs["housing"]/10)){
        if(sample(1:100, 1) <= 50){
          place_building(city_center, city_center, "residential", 10)
          meta_needs["housing"] <<- meta_needs["housing"]-10
        }
      }
    }

    
    # while(needs["food"] > 0){
    #   place_building(city_center, city_center, "commercial", 0, "food", 100, 10)
    #   needs["food"] <<- needs["food"]-100
    # }
    # #
    # # while(needs["food"] > 0){
    # #   place_building(city_center, city_center, "commercial", 0, "food", 100, 10)
    # #   needs["food"] <<- needs["food"]-100
    # # }
    # #
    # # trade
    # 
    # while(trade["food"] < 0){
    #   place_building(city_center, city_center, "industrial", 0, "food", 50, 20)
    #   trade["food"] <<- trade["food"]+50
    # }
    
    # for(goods in names(commercial_preset)){
    #   while(needs[goods] > 0){
    #     place_building(city_center, city_center, "commercial", 0, goods, commercial_preset[goods], 10)
    #     needs[goods] <<- needs[goods] - commercial_preset[goods]
    #   }
    # }
    # 
    # for(goods in names(industrial_preset)){
    #   while(trade[goods] < 0){
    #     place_building(city_center, city_center, "industrial", 0, goods, industrial_preset[goods], 20)
    #     trade[goods] <<- trade[goods] + industrial_preset[goods]
    #   }
    # }

    for(goods in names(commercial_preset)){
      if(needs[goods] > 0){
        for(i in as.integer(needs[goods] / commercial_preset[goods])){
          if(sample(1:100, 1) <= 10){
            place_building(city_center, city_center, "commercial", 0, goods, commercial_preset[goods], 10)
            needs[goods] <<- needs[goods] - commercial_preset[goods]
          }
        }
      }
    }

    for(goods in names(industrial_preset)){
      if(trade[goods] < 0){
        for(i in as.integer(-trade[goods] / industrial_preset[goods])){
          if(sample(1:100, 1) <= 10){
            place_building(city_center, city_center, "industrial", 0, goods, industrial_preset[goods], 20)
            trade[goods] <<- trade[goods] + industrial_preset[goods]
          }
        }
      }
    }
    
    
    ##### PEOPLE #####
    
    # # birth rate
    # birth_rate_pool <<- birth_rate_pool + length(citizens)*0.02 # 2% growth
    #
    # births <- floor(birth_rate_pool)
    # birth_rate_pool <<- birth_rate_pool - floor(birth_rate_pool)
    
    death_count <<- list(Age = 0, Homeless = 0)
    birth_count <<- 0
    
    # aging citizens
    for(row in 1:nrow(city)){
      for(col in 1:ncol(city)){
        if(!is.null(city[[row, col]])){
          if(length(city[[row, col]]$residents) > 0){
            
            deaths <- c()
            for(res in 1:length(city[[row, col]]$residents)){
              # age
              city[[row, col]]$residents[[res]]$age <<- city[[row, col]]$residents[[res]]$age + 1
              
              # deaths
              person <- city[[row, col]]$residents[[res]]
              if(person$age > 100) {
                deaths <- c(deaths, res)
                if(COMPLEX_LOG)
                  cat("death: (name: ", person$name, ")\n")
              } else if(person$age > 85 & sample(1:100, 1) <= 50) {
                deaths <- c(deaths, res)
                if(COMPLEX_LOG)
                  cat("death: (name: ", person$name, ")\n")
              } else if(person$age > 70 & sample(1:100, 1) <= 20) {
                deaths <- c(deaths, res)
                if(COMPLEX_LOG)
                  cat("death: (name: ", person$name, ")\n")
              }
            }
            death_count$Age <<- death_count$Age + length(deaths)
            
            # remove dead
            if(length(deaths) > 0){
              city[[row, col]]$residents <<- city[[row, col]]$residents[-deaths]
            }
          }
        }
      }
    }
    
    # aging homeless
    if(length(homeless) > 0){
      
      deaths <- c()
      for(res in 1:length(homeless)){
        # age
        homeless[[res]]$age <<- homeless[[res]]$age + 1
        
        # deaths
        person <- homeless[[res]]
        
        if(sample(1:10, 1) == 1){
          deaths <- c(deaths, res)
          death_count$Homeless <<- death_count$Homeless + 1
          if(COMPLEX_LOG)
            cat("death: (name: ", person$name, ")\n")
        } else if(person$age > 100) {
          deaths <- c(deaths, res)
          if(COMPLEX_LOG)
            cat("death: (name: ", person$name, ")\n")
        } else if(person$age > 85 & sample(1:100, 1) <= 50) {
          deaths <- c(deaths, res)
          if(COMPLEX_LOG)
            cat("death: (name: ", person$name, ")\n")
        } else if(person$age > 70 & sample(1:100, 1) <= 20) {
          deaths <- c(deaths, res)
          if(COMPLEX_LOG)
            cat("death: (name: ", person$name, ")\n")
        }
      }
      death_count$Age <<- death_count$Age + (length(deaths) - death_count$Homeless)
      
      # remove dead
      if(length(deaths) > 0){
        homeless <<- homeless[-deaths]
      }
    }
    
    # recalculate citizens
    citizens <<- c()
    for(building in city){
      if(!is.null(building)){
        citizens <<- c(citizens, building$residents)
      }
    }
    citizens <<- c(citizens, homeless)
    
    # births
    for(person in citizens){
      if(person$age > 18 & person$age < 50){
        if(sample(1:100, 1) <= 4) { # 4% chance
          homeless <<- c(homeless, list(make_citizen(names[[(sample(1:length(names), 1))]], 0)))
          birth_count <<- birth_count + 1
          if(COMPLEX_LOG)
            cat("birth: (name: ", homeless[[length(homeless)]]$name, ")\n")
        }
      }
    }
    
    # relocation
    if(length(homeless) > 0){
      for(row in 1:nrow(city)){
        for(col in 1:ncol(city)){
          if(!is.null(city[[row, col]])){
            if(city[[row, col]]$type == "residential"){
              while(length(homeless) > 0 & length(city[[row, col]]$residents) < 10){
                city[[row, col]]$residents <<- c(city[[row, col]]$residents, homeless[1])
                homeless <<- homeless[-1]
              }
            }
          }
        }
      }
    }
    # recalculate citizens
    citizens <<- c()
    for(building in city){
      if(!is.null(building)){
        citizens <<- c(citizens, building$residents)
      }
    }
    citizens <<- c(citizens, homeless)
    
    # Save to dataframe
    
    DataSet[tick_num, ] <<- list(
      Tick = tick_num,
      Population = length(citizens),
      Buildings = sum(!sapply(city, is.null)),
      # Residential_buildings = sum(city[!sapply(city, is.null)][type=="Residential"]),
      # Residential_buildings = sum(sapply(city, function(x) {if(!is.null(x) & x$type=="Residential"){TRUE} else {FALSE}})),
      Residential_buildings = sum(sapply(city, function(x) {!is.null(x) && x$type=="residential"})),
      Commercial_buildings = sum(sapply(city, function(x) {!is.null(x) && x$type=="commercial"})),
      # Industrial_buildings = sum(!sapply(city, function(x) {is.null(x) & x$type="Industrial"})),
      Industrial_buildings = sum(sapply(city, function(x) {!is.null(x) && x$type=="industrial"})),
      Births = birth_count,
      Age_Deaths = death_count$Age,
      Homeless_Deaths = death_count$Homeless
    )
    
    tick_num <<- tick_num + 1
  }
}

# # plotting graphs
# graph() <- function(data) {
#
#
#   text(x = barplot(data, names.arg = DataSet$Tick, xlab = "Tick", ylab = "Population", ylim = c(0, max(DataSet$Population)*1.2)),
#        y = data + 1, labels = data)
# }


##### MAIN #####


# UI
main_UI = "\014
██████████████████████████████████
█           Table City           █
██████████████████████████████████
Command list:

clear - clears the console
end - terminates the app
new_city - creates a new city
progress - progresses the city by ... ticks
citizens - creates and shows a table of citizens
buildings - creates and shows a table of buldings
visualize - plots city statistics graph over ticks
sim - progress and visualize
map - city matrix visualization


command: "

main <- function(){
  cat(main_UI)
  
  # UI loop
  while(TRUE){
    command = tolower(readline())
    
    if(command == "clear"){
      cat(main_UI)
      
    } else if(command == "end"){
      break
      
    } else if(command == "new_city"){
      new_city(as.integer(readline("city size: ")))
      cat("New city created!")
      
    } else if(command %in% c("progress", "prog")){
      tryCatch({command = as.integer(readline("ticks: "))}, error = function(e) {command = 0})
      
      if(!is.na(command) && command > 0){
        for(tick in 1:command){
          
          #progress
          timestamp <- as.numeric(Sys.time())
          progress(1)
          
          #print
          cat(paste0("Tick: ", tick, " / ", command, " (", round((as.numeric(Sys.time()) - timestamp)*1000), "ms) \n\n"))
        }
      }
      
    } else if(command == "citizens"){
      
      names_vec <- sapply(citizens, function(x) x$name) # goes through citizens one by one, extracts name and converts into simple list (sapply)
      ages_vec <- sapply(citizens, function(x) x$age)
      
      citizens_df <- data.frame(Name = names_vec, Age = ages_vec)
      citizens_df <- citizens_df[order(citizens_df$Age), ]
      
      print(citizens_df, row.names = FALSE)
      
    } else if(command == "buildings"){
      
      types_vec <- sapply(city, function(x) {if(is.null(x$type)) {"empty"} else {x$type}})
      residents_vec <- sapply(city, function(x) {if(is.null(x) || x$resident_limit == 0) {"-"} else {paste0(length(x$residents), "/", x$resident_limit)}})
      # product_vec <- sapply(city, function(x) x$product)
      # production_vec <- sapply(city, function(x) x$production)
      production_vec <- sapply(city, function(x) {if(is.null(x$product)) {"-"} else {paste0(x$product, "(", x$production, ")")}})
      positions_vec <- sapply(city, function(x) {if(is.null(x) || x$positions == 0) {"-"} else {x$positions}})
      
      city_df <- data.frame(Type = types_vec, Residents = residents_vec, Production = production_vec, Positions = positions_vec)
      city_df <- subset(city_df, !(Type == "empty"))
      
      print(city_df, row.names = FALSE)
      
    } else if(command %in% c("visualize", "vis")){
      cat("
City
Population
Buildings
Supply
Trade

Select: ")
      command = tolower(readline())
      
      if(command %in% c("population", "pop")) {
        # text(x = barplot(DataSet$Population, names.arg = DataSet$Tick, xlab = "Tick", ylab = "Population", ylim = c(0, max(DataSet$Population)*1.2)),
        #      y = DataSet$Population + 1, labels = DataSet$Population)
        
        plot(DataSet$Population, col = "Black", type="l", xlab = "Tick", ylab = "", main = paste0("Tick ", tick_num), ylim = c(0, max(c(DataSet$Population, DataSet$Buildings))))
        lines(DataSet$Births, col = "Green")
        lines(DataSet$Age_Deaths, col = "Red")
        lines(DataSet$Homeless_Deaths, col = "DarkRed")
        
        legend("topleft",
               legend = c("Population", "Births", "Age related deaths", "Homelessness related deaths"),
               fill = c("Black", "Green", "Red", "DarkRed"))
      }
      else if(command %in% c("buildings", "build")) {
        # text(x = barplot(DataSet$Buildings, names.arg = DataSet$Tick, xlab = "Tick", ylab = "Buildings", ylim = c(0, max(DataSet$Population)*1.2)),
        #      y = DataSet$Buildings + 1, labels = DataSet$Buildings)
        
        plot(DataSet$Buildings, col = "Black", type="l", xlab = "Tick", ylab = "", main = paste0("Tick ", tick_num), ylim = c(0, max(c(DataSet$Buildings, DataSet$Buildings))))
        lines(DataSet$Residential_Buildings, col = "Green")
        lines(DataSet$Commercial_Buildings, col = "Blue")
        lines(DataSet$Industrial_Buildings, col = "Orange")
        
        legend("topleft",
               legend = c("Buildings", "Residential", "Commercial", "Industrial"),
               fill = c("Black", "Green", "Blue", "Orange"))
      }
      else if(command %in% c("all", "city")) {
        plot(DataSet$Population, col = "Red", type="l", xlab = "Tick", ylab = "", main = paste0("Tick ", tick_num), ylim = c(0, max(c(DataSet$Population, DataSet$Buildings))))
        abline(h = city_size^2, col = "black")
        lines(DataSet$Buildings, col = "Green")
        
        legend("topleft",
               legend = c("Population", "Buildings"),
               fill = c("Red", "Green"))
      }
      else if(command == "supply") {  
        # data <- rbindlist(list(supply,
        #                        demand),
        #                   idcol = TRUE)  
        # data
        # input()
        
        data <- as.data.frame(
          do.call(rbind, list(supply = supply, demand = demand))
          )
        
        print(t(as.matrix(data)))
        
        barplot(as.matrix(data),
                col = c("green", "red"),
                beside = TRUE,
                # legend.text = TRUE,
                legend.text = c("Supply", "Demand"),
                main = "Supply / Demand",
                xlab = "Goods",
                ylab = "",
                las = 1)
        
        # barplot(t(as.matrix(data)), beside=TRUE)
      }
      else if(command == "trade") {
        data <- as.data.frame(
          do.call(rbind, list(export = export, import = import))
        )
        
        print(t(as.matrix(data)))
        
        barplot(as.matrix(data),
                col = c("green", "red"),
                beside = TRUE,
                # legend.text = TRUE,
                legend.text = c("Export", "Import"),
                main = "Import / Export",
                xlab = "Goods",
                ylab = "",
                las = 1)
      }
      
    } else if(command == "sim"){
      tryCatch({command = as.integer(readline("ticks: "))}, error = function(e) {command = 0})
      
      if(!is.na(command) && command > 0){
        for(tick in 1:command){
          
          #progress
          timestamp <- as.numeric(Sys.time())
          progress(1)
          
          #print
          # cat(paste0("Tick: ", tick, " / ", command, "\n\n"))
          cat(paste0("Tick: ", tick, " / ", command, " (", round((as.numeric(Sys.time()) - timestamp)*1000), "ms) \n\n"))
          
          #sleep
          Sys.sleep(0.1)
          
          #visualize
          if(tick %% 10 == 0){
            plot(DataSet$Population, col = "Red", type="l", xlab = "Tick", ylab = "", main = paste0("Tick ", tick_num), ylim = c(0, max(c(DataSet$Population, DataSet$Buildings))))
            abline(h = city_size^2, col = "black")
            lines(DataSet$Buildings, col = "Green")
            
            legend("topleft",
                   legend = c("Population", "Buildings"),
                   fill = c("Red", "Green"))
          }
          
        }}
      
    } else if(command == "map"){
      
      # map <- matrix(0, nrow = nrow(city), ncol = ncol(city))
      # 
      # #map <- city
      # map[is.null(city)] <- 0
      # map[city$type == "Town_hall"] <- 1
      # map[city$type == "Residential"] <- 2
      # map[city$type == "Commercial"] <- 3
      # 
      # image(t(map[nrow(map):1, ]), 
      #       col = c("gray", "black", "blue", "green"), 
      #       axes = FALSE,
      #       useRaster = TRUE) # Keeps pixels sharp
      
      map <- matrix(
        sapply(city, function(x) {
          if (is.null(x)) {
            return(0)
          } else if (x$type == "Town_hall") {
            return(1)
          } else if (x$type == "residential") {
            return(2)
          } else if (x$type == "commercial") {
            return(3)
          } else if (x$type == "industrial") {
            return(4)
          }else {
            return(0)
          }
        }), 
        nrow = nrow(city), 
        ncol = ncol(city)
      )
      
      image(t(map[nrow(map):1, ]),
            col = c("gray", "black", "green", "blue", "orange"),
            axes = FALSE,
            useRaster = TRUE,
            main = paste0("City matrix at tick ", tick_num))
      
      legend("topright", 
             legend = c("Residential", "Commercial", "Industrial"), 
             fill = c("green", "blue", "orange"))
      
    }else {
      cat("Unknown command\n")
    }
  }
}



names <- c("Ondřej", "Jan", "Jiří", "Josef", "Pavel", "Martin", "Jaroslav", "Petr", "Tomáš", "František", "Miroslav", "Jana", "Marie", "Eva", "Hana", "Anna", "Lenka", "Věra", "Alena", "Lucie", "Petra")

# Startup
# city_center <- as.integer(city_size/2)
# city <- matrix(list(), nrow=city_size, ncol=city_size)
# city[[city_center, city_center]] <- make_building("Town_hall")

new_city(INIT_CITYSIZE)
tick_num <- 0

meta_demand <- c("housing" = 0, "positions" = 0)
# demand <- c("food" = 0)
# demand <- commercial_preset * 0


main()



# Debug
# populate_buildings(city_center, city_center, "residential", 8)

#table(list())
