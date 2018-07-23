$(() => {
  function makeVideoUrl(videoId) {
    return `https://youtu.be/${videoId}`;
  }

  function makeImageUrl(videoId) {
    return `https://img.youtube.com/vi/${videoId}/maxresdefault.jpg`;
  }

  function makeNoEmbedUrl(videoId) {
    return `https://noembed.com/embed?url=https://www.youtube.com/watch?v=${videoId}`;
  }

  const videos = [
    { id: "WKiLkGBcXRU", episode: 0 },
    { id: "iNeLpmjowwQ" },
    { id: "cDYn_la-9vg" },
    { id: "hp-uQZ-MujA" },
    { id: "uB7ES5-JhQA" },
    { id: "vcE9BlW74tg" },
    { id: "sP6Iuwd_Nik" },
    { id: "9qiRk5la724" }
  ];

  const divContainer = $("#carousel .carousel-inner");
  const divTemplate = $("#carousel-template-image");

  for (let i = 0; i < videos.length; ++i) {
    const video = videos[i];
    const videoId = video.id;

    const div = $(divTemplate.html());
    div.attr("id", `video-${videoId}`);

    const anchor = div.find("> :first-child");
    anchor.attr("href", makeVideoUrl(videoId));

    const image = anchor.find("> :first-child");
    image.attr("src", makeImageUrl(videoId));

    if (i == 0) {
      div.addClass("active");
    }

    divContainer.append(div);

    $.get({
      url: makeNoEmbedUrl(videoId),
      dataType: "json"
    }).then(response => {
      const slug = typeof video.episode != "undefined" ? `usethetypes#${video.episode}` : "test";
      const title = `${slug}: ${response.title}`;
      anchor
        .attr("title", title)
        .attr("alt", title);
      div.find("p").text(title);
    });
  }
});
