
## Region intensities

region_intensity = function(image_no){
  
  
  
  # Core
  
  
  ## All
  im_core = combdata_reg %>% 
    filter(imageID_new == unique(combdata_reg$imageID_new)[image_no],
           region == "core")
  im_core_intensity = ppp(im_core$x, im_core$y,
                          window = makeWindowSmall(window = "concave", window.length = 2,
                                                   data = im_core)
  ) %>% intensity()
  
  
  ## Melanoma
  im_core_mel = combdata_reg %>% 
    filter(imageID_new == unique(combdata_reg$imageID_new)[image_no],
           region == "core",
           melanoma_sox10 == 1 | melanoma_pd_l1 == 1)
  im_core_mel_intensity = ppp(im_core_mel$x, im_core_mel$y,
                              window = makeWindowSmall(window = "concave", window.length = 2,
                                                       data = im_core)
  ) %>% intensity()
  
  ## Filtering data
  
  
  
  # Edge
  
  
  
  ## All
  im_edge = combdata_reg %>% 
    filter(imageID_new == unique(combdata_reg$imageID_new)[image_no],
           region == "edge")
  im_edge_intensity = ppp(im_edge$x, im_edge$y,
                          window = makeWindowSmall(window = "concave", window.length = 2,
                                                   data = im_edge)
  ) %>% intensity()
  
  
  ## Melanoma
  im_edge_mel = combdata_reg %>% 
    filter(imageID_new == unique(combdata_reg$imageID_new)[image_no],
           region == "edge",
           melanoma_sox10 == 1 | melanoma_pd_l1 == 1)
  im_edge_mel_intensity = ppp(im_edge_mel$x, im_edge_mel$y,
                              window = makeWindowSmall(window = "concave", window.length = 2,
                                                       data = im_edge)
  ) %>% intensity()
  
  
  
  # Margin
  
  
  
  ## All
  im_margin = combdata_reg %>% 
    filter(imageID_new == unique(combdata_reg$imageID_new)[image_no],
           region == "margin")
  im_margin_intensity = ppp(im_margin$x, im_margin$y,
                            window = makeWindowSmall(window = "concave", window.length = 2,
                                                     data = im_margin)
  ) %>% intensity()
  
  
  ## Melanoma
  im_margin_mel = combdata_reg %>% 
    filter(imageID_new == unique(combdata_reg$imageID_new)[image_no],
           region == "margin",
           melanoma_sox10 == 1 | melanoma_pd_l1 == 1)
  im_margin_mel_intensity = ppp(im_margin_mel$x, im_margin_mel$y,
                                window = makeWindowSmall(window = "concave", window.length = 2,
                                                         data = im_margin)
  ) %>% intensity()
  
  
  
  # Nonmelanoma
  
  
  
  ## All
  im_nonmel = combdata_reg %>% 
    filter(imageID_new == unique(combdata_reg$imageID_new)[image_no],
           region == "nonmelanoma")
  im_nonmel_intensity = ppp(im_nonmel$x, im_nonmel$y,
                            window = makeWindowSmall(window = "concave", window.length = 2,
                                                     data = im_nonmel)
  ) %>% intensity()
  
  
  ## Melanoma
  im_nonmel_mel = combdata_reg %>% 
    filter(imageID_new == unique(combdata_reg$imageID_new)[image_no],
           region == "nonmelanoma",
           melanoma_sox10 == 1 | melanoma_pd_l1 == 1)
  im_nonmel_mel_intensity = ppp(im_nonmel_mel$x, im_nonmel_mel$y,
                                window = makeWindowSmall(window = "concave", window.length = 2,
                                                         data = im_nonmel)
  ) %>% intensity()
  
  
  
  
  return(list("core_all" = im_core_intensity,
              "core_melanoma" = im_core_mel_intensity,
              "edge" = im_edge_intensity,
              "edge_melanoma" = im_edge_mel_intensity,
              "margin" = im_margin_intensity,
              "margin_melanoma" = im_margin_mel_intensity,
              "nonmelanoma" = im_nonmel_intensity,
              "nonmelanoma_melanoma" = im_nonmel_mel_intensity)
  )
}