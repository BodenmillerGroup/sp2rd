.get_spatial <- function(object, coords, img_id, margin){
    
    if (is(object, "SpatialExperiment")) {
        spatial_df <- as.data.frame(spatialCoords(object)[,coords])
    } else {
        spatial_df <- colData(object)[,coords]
    }
    
    # Concatenate images and add padding
    ni <- length(unique(colData(object)[[img_id]]))
    if (ni > 1) {
        image_ids <- unique(colData(object)[[img_id]])
        
        nc <- ceiling(sqrt(ni))
        nr <- ceiling(ni/nc) 
        
        spatial_df$img_id <- colData(object)[[img_id]]
        max_measures <- spatial_df %>%
            group_by(img_id) %>%
            summarize(width = max(!!sym(coords[1])) - min(!!sym(coords[1])),
                      height = max(!!sym(coords[2])) - min(!!sym(coords[2]))) %>%
            ungroup() %>%
            summarize(max_width = max(width),
                      max_height = max(height))
        
        max_width <- max_measures$max_width + margin
        max_height <- max_measures$max_height + margin
        
        image_mapping <- data.frame(img_id = image_ids,
                                    nx = rep_len(seq_len(nc), length.out = ni) - 1,
                                    ny = rep_len(rep(seq_len(nr), each = nc), length.out = ni) - 1)
        
        spatial_df <- merge(spatial_df, image_mapping, by = "img_id", sort = FALSE)
        spatial_df[,coords[1]] <- spatial_df[,coords[1]] + max_width * spatial_df$nx
        spatial_df[,coords[2]] <- spatial_df[,coords[2]] + max_height * spatial_df$nx
    }
    
    return(spatial_df)
}
