
package com.gitlab.vladimirp1.nasa;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "nasa_id",
    "date_created",
    "title",
    "media_type",
    "description",
    "center",
    "photographer",
    "keywords",
    "location",
    "description_508"
})
public class Datum {

    @JsonProperty("nasa_id")
    private String nasaId;
    @JsonProperty("date_created")
    private String dateCreated;
    @JsonProperty("title")
    private String title;
    @JsonProperty("media_type")
    private String mediaType;
    @JsonProperty("description")
    private String description;
    @JsonProperty("center")
    private String center;
    @JsonProperty("photographer")
    private String photographer;
    @JsonProperty("keywords")
    private List<String> keywords = null;
    @JsonProperty("location")
    private String location;
    @JsonProperty("description_508")
    private String description508;
    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    @JsonProperty("nasa_id")
    public String getNasaId() {
        return nasaId;
    }

    @JsonProperty("nasa_id")
    public void setNasaId(String nasaId) {
        this.nasaId = nasaId;
    }

    @JsonProperty("date_created")
    public String getDateCreated() {
        return dateCreated;
    }

    @JsonProperty("date_created")
    public void setDateCreated(String dateCreated) {
        this.dateCreated = dateCreated;
    }

    @JsonProperty("title")
    public String getTitle() {
        return title;
    }

    @JsonProperty("title")
    public void setTitle(String title) {
        this.title = title;
    }

    @JsonProperty("media_type")
    public String getMediaType() {
        return mediaType;
    }

    @JsonProperty("media_type")
    public void setMediaType(String mediaType) {
        this.mediaType = mediaType;
    }

    @JsonProperty("description")
    public String getDescription() {
        return description;
    }

    @JsonProperty("description")
    public void setDescription(String description) {
        this.description = description;
    }

    @JsonProperty("center")
    public String getCenter() {
        return center;
    }

    @JsonProperty("center")
    public void setCenter(String center) {
        this.center = center;
    }

    @JsonProperty("photographer")
    public String getPhotographer() {
        return photographer;
    }

    @JsonProperty("photographer")
    public void setPhotographer(String photographer) {
        this.photographer = photographer;
    }

    @JsonProperty("keywords")
    public List<String> getKeywords() {
        return keywords;
    }

    @JsonProperty("keywords")
    public void setKeywords(List<String> keywords) {
        this.keywords = keywords;
    }

    @JsonProperty("location")
    public String getLocation() {
        return location;
    }

    @JsonProperty("location")
    public void setLocation(String location) {
        this.location = location;
    }

    @JsonProperty("description_508")
    public String getDescription508() {
        return description508;
    }

    @JsonProperty("description_508")
    public void setDescription508(String description508) {
        this.description508 = description508;
    }

    @JsonAnyGetter
    public Map<String, Object> getAdditionalProperties() {
        return this.additionalProperties;
    }

    @JsonAnySetter
    public void setAdditionalProperty(String name, Object value) {
        this.additionalProperties.put(name, value);
    }

}
