#' Animation of location to reduced dimension transitions
#'
#' TODO
#'
#' @param object a \code{SingleCellExperiment} or \code{SpatialExperiment} object
#'
#' @return TODO
#'
#' @examples
#' # TODO
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#'
#' @importFrom tweenr tween_states
#' @importFrom gganimate transition_states
#' @export
spatial2dimred <- function(object,
                           coords = c("Pos_X", "Pos_Y"),
                           color_by = NULL,
                           img_id = "sample_id",
                           dimred = "UMAP",
                           margin = 100){
    
    # Get coordinates
    spatial_df <- .get_spatial(object, coords, img_id, margin)
    
    dimred_df <- reducedDim(object, dimred)
    
    # Scale dimred to image
    min_x <- min(spatial_df[,coords[1]])
    max_x <- max(spatial_df[,coords[1]])
    min_y <- min(spatial_df[,coords[2]])
    max_y <- max(spatial_df[,coords[2]])
    
    dimred_df[,1] <- rescale(dimred_df[,1], to = c(min_x, max_x))
    dimred_df[,2] <- rescale(dimred_df[,2], to = c(min_y, max_y))
    
    state_1 <- data.frame(x = spatial_df[,coords[1]],
                          y = spatial_df[,coords[2]])
    state_2 <- data.frame(x = dimred_df[,1],
                          y = dimred_df[,2])
    
    state_1[[color_by]] <- state_2[[color_by]] <- colData(object)[[color_by]]
    
    state_list <- list(state_1, state_2, state_1)
    
    tweened_states <- tween_states(state_list, 
                                   tweenlength = 2, 
                                   statelength = 1,
                                   ease = "cubic-in-out",
                                   nframes = 96)
    
    ggplot(tweened_states, aes(x, y, color = .data[[color_by]])) + 
        geom_point() + 
        transition_states(
            .frame,
            transition_length = 2,
            state_length = 1
        ) +
        enter_fade() + 
        exit_shrink() +
        ease_aes('sine-in-out') +
        theme_void()
}