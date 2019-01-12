$(() => {
  function makeYouTubeUrl(video) {
    return `https://youtu.be/${video.youTubeId}`;
  }

  function makeImageUrl(video) {
    return `https://img.youtube.com/vi/${video.youTubeId}/maxresdefault.jpg`;
  }

  Array.prototype.rotate = function(n) {
    return this.slice(n, this.length).concat(this.slice(0, n));
  }

  function randomInt(min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
  }

  $.get({
    url: `/videos.json?${TAG}`,
    dataType: "json"
  }).then(videos => {
    const container = $("#carousel .carousel-inner");
    const divTemplate = $("#carousel-image-template");

    for (const video of videos.rotate(randomInt(0, videos.length))) {
      const youTubeUrl = makeYouTubeUrl(video);

      const div = $(divTemplate.html());

      const a = div.find("> :first-child");
      a.attr("href", youTubeUrl);

      const img = a.find("> :first-child");
      img.attr("src", makeImageUrl(video));

      container.append(div);

      const innerDiv = div.find("div:first");
      a
        .attr("title", video.title)
        .attr("alt", video.title);
      innerDiv.find("a:first")
        .attr("href", youTubeUrl)
        .text(video.title);
      innerDiv.find("a:eq(1)")
        .attr("href", video.href);
    }

    container.find("div:first").addClass("active");
  });
});
