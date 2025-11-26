import { defineConfig } from 'vitepress'

export default defineConfig({
  base: '/proteus/',
  title: "Proteus",
  description: "Protobuf library for Scala",
  head: [['link', { rel: 'icon', href: '/proteus/favicon.png' }]],

  themeConfig: {
    nav: [
      { text: 'Home', link: '/' },
      { text: 'Documentation', link: '/getting-started' },
      { text: 'FAQ', link: '/faq' },
    ],

    logo: '/proteus.svg',

    sidebar: [
      {
        text: 'Documentation',
        items: [
          { text: 'Getting started', link: '/getting-started' },
          { text: 'Customization', link: '/customization' },
          { text: 'gRPC services', link: '/grpc-services' },
          { text: 'Proto file generation', link: '/proto-file-generation' },
          { text: 'JSON support', link: '/json-support' },
        ]
      },
      {
        text: 'FAQ', link: '/faq'
      }
    ],

    socialLinks: [
      { icon: 'github', link: 'https://github.com/ghostdogpr/proteus' }
    ],

    search: {
      provider: 'local'
    }
  }
})
